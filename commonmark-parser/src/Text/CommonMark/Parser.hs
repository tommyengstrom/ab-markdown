{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
module Text.CommonMark.Parser
    ( commonmarkToDoc
    ) where

import           Control.Applicative
import           Control.Bool
import           Control.Monad
import           Control.Monad.Trans.RWS.Strict
import           Data.Foldable                     (toList)
import           Data.List                         (intercalate)
import qualified Data.Map                          as M
import           Data.Maybe                        (mapMaybe)
import           Data.Monoid
import           Data.Sequence                     (Seq, ViewR (..), singleton,
                                                    viewr, (<|), (><), (|>))
import qualified Data.Sequence                     as Seq
import qualified Data.Set                          as Set
import           Data.Text.Extended                (Text)
import qualified Data.Text.Extended                as T
import           Prelude                           hiding (takeWhile)

import           Data.Either

import           Text.CommonMark.Parser.Inline
import           Text.CommonMark.Parser.Options
import           Text.CommonMark.Parser.Util
import           Text.CommonMark.ParserCombinators
import           Text.CommonMark.Syntax
import           Text.CommonMark.Types

import           Debug.Trace

commonmarkToDoc :: ParseOptions -> Text -> Doc Text
commonmarkToDoc opts text = Doc $ processDocument (cont, opts')
    where (cont, refmap) = processLines text
          opts' = opts { parseOptLinkReferences
                            = liftA2 (<|>) (parseOptLinkReferences opts)
                                           (lookupLinkReference refmap) }

-- General parsing strategy:
--
-- Step 1:  processLines
--
-- We process the input line by line.  Each line modifies the
-- container stack, by adding a leaf to the current open container,
-- sometimes after closing old containers and/or opening new ones.
--
-- To open a container is to add it to the top of the container stack,
-- so that new content will be added under this container.
-- To close a container is to remove it from the container stack and
-- make it a child of the container above it on the container stack.
--
-- When all the input has been processed, we close all open containers
-- except the root (Document) container.  At this point we should also
-- have a ReferenceMap containing any defined link references.
--
-- Step 2:  processDocument
--
-- We then convert this container structure into an AST.  This principally
-- involves (a) gathering consecutive ListItem containers into lists, (b)
-- gathering TextLine nodes that don't belong to verbatim containers into
-- paragraphs, and (c) parsing the inline contents of non-verbatim TextLines.

--------

-- Container stack definitions:
data ContainerStack = ContainerStack Container {- top -} [Container] {- rest -}

type LineNumber = Int

data Elt = C Container
         | L LineNumber Leaf
         deriving (Show)

data Container = Container { containerType :: ContainerType
                           , children      :: Seq Elt
                           }

data ContainerType = Document
                   | BlockQuote
                   | ListItem { padding  :: Int
                              , itemType :: ListType
                              }
                   | FencedCode { startColumn :: Int
                                , fence       :: Text
                                , info        :: Maybe Text
                                }
                   | IndentedCode
                   | RawHtmlBlock
                   | Reference
                   deriving (Show, Eq)

isIndentedCode :: Elt -> Bool
isIndentedCode (C (Container IndentedCode _)) = True
isIndentedCode _                              = False

isBlankLine :: Elt -> Bool
isBlankLine (L _ BlankLine{}) = True
isBlankLine _ = False

isTextLine :: Elt -> Bool
isTextLine (L _ (TextLine _)) = True
isTextLine _ = False

isListItem :: ContainerType -> Bool
isListItem ListItem{} = True
isListItem _ = False

instance Show Container where
    show c = show (containerType c) ++ "\n" ++
                nest 2 (intercalate "\n" (map pptElt $ toList $ children c))

nest :: Int -> String -> String
nest num = intercalate "\n" . map (replicate num ' ' ++) . lines

pptElt :: Elt -> String
pptElt (C c) = show c
pptElt (L _ (TextLine s)) = show s
pptElt (L _ lf) = show lf

-- Scanners that must be satisfied if the current open container is to be
-- continued on a new line (ignoring lazy continuations).
containerContinue :: Container -> Scanner
containerContinue c = case containerType c of
    BlockQuote     -> scanNonindentSpace *> scanBlockquoteStart
    IndentedCode   -> scanIndentSpace
    FencedCode{..} -> scanSpacesUpToColumn startColumn
    RawHtmlBlock   -> nfb scanBlankline
    ListItem{..}   -> scanBlankline <|> (() <$ count padding (char ' '))
    -- TODO: This is likely to be incorrect behaviour. Check.
    Reference -> nfb (scanBlankline <|> (scanNonindentSpace *> (scanReference
                                                            <|> scanBlockquoteStart
                                                            <|> scanHRuleLine))
                                    <|> (() <$ parseAtxHeaderStart))
    _              -> pure ()
{-# INLINE containerContinue #-}

-- Defines parsers that open new containers.
containerStart :: Bool -> Bool -> Parser ContainerType
containerStart afterListItem _lastLineIsText = choice
    [ scanNonindentSpace *> pure BlockQuote <* scanBlockquoteStart
    , parseListMarker afterListItem
    ]

-- Defines parsers that open new verbatim containers (containers
-- that take only TextLine and BlankLine as children).
verbatimContainerStart :: Bool -> Parser ContainerType
verbatimContainerStart lastLineIsText = choice
   [ scanNonindentSpace *> parseCodeFence
   , guard (not lastLineIsText) *> scanNonindentSpace *> (IndentedCode <$ char ' ' <* nfb scanBlankline)
   , RawHtmlBlock <$ scanHtmlBlockStart
   , guard (not lastLineIsText) *> scanNonindentSpace *> (Reference <$ scanReference)
   ]

-- Leaves of the container structure (they don't take children).
type Leaf = GenLeaf Text
data GenLeaf t = TextLine         t
               | BlankLine        t
               | ATXHeader    Int t
               | SetextHeader Int t
               | SetextToken  Int t
               | Rule
               deriving (Show, Functor)

type ContainerM = RWS () ReferenceMap ContainerStack

-- Close the whole container stack, leaving only the root Document container.
closeStack :: ContainerM Container
closeStack = get >>= \case ContainerStack top [] -> return top
                           ContainerStack _ _    -> closeContainer >> closeStack

-- Close the top container on the stack.  If the container is a Reference
-- container, attempt to parse the reference and update the reference map.
-- If it is a list item container, move a final BlankLine outside the list
-- item.
closeContainer :: ContainerM ()
closeContainer = do
  ContainerStack top rest <- get
  case top of
    (Container Reference cs'') ->
      case parse ((,) <$> pReference <*> takeText) (T.strip $ T.joinLines $ map extractText $ toList textlines) of
           Right ((lab, lnk, tit), unconsumed) -> do
             tell (M.singleton (normalizeReference lab) (lnk, tit))
             case rest of
               (Container ct' cs' : rs)
                 | T.null unconsumed -> put $ ContainerStack (Container ct' (rest' <> cs' |> C top)) rs
                 | otherwise         -> put $ ContainerStack (Container ct' ((L (-1) (TextLine unconsumed) <| rest') >< (cs' |> C top))) rs
               [] -> return ()
           Left _ ->
             case rest of
               (Container ct' cs' : rs) ->
                   put $ ContainerStack (Container ct' (cs' <> cs'')) rs
               [] -> return ()
      where (textlines, rest') = Seq.spanl isTextLine cs''
    (Container li@ListItem{} cs'') ->
      case rest of
        -- move final BlankLine outside of list item
        (Container ct' cs' : rs) ->
          case viewr cs'' of
            (zs :> b) | isBlankLine b -> put $ ContainerStack (Container ct' els) rs
                where els | null zs = cs' |> C (Container li zs)
                          | otherwise   = cs' |> C (Container li zs) |> b

            _ -> put $ ContainerStack (Container ct' (cs' |> C top)) rs
        [] -> return ()
    _ -> case rest of
          (Container ct' cs' : rs) ->
              put $ ContainerStack (Container ct' (cs' |> C top)) rs
          [] -> return ()

-- Add a leaf to the top container.
addLeaf :: LineNumber -> Leaf -> ContainerM ()
addLeaf lineNum lf = do
  ContainerStack top rest <- get
  case (top, lf) of
    (Container ct@(ListItem{}) cs, BlankLine{})
      | (_ :> secondLine) <- viewr cs -- two blanks break out of list item:
      , isBlankLine secondLine
       -> closeContainer >> addLeaf lineNum lf
      | otherwise
       -> put $ ContainerStack (Container ct (cs |> L lineNum lf)) rest
    (Container ct cs, _) ->
      put $ ContainerStack (Container ct (cs |> L lineNum lf)) rest

-- Add a container to the container stack.
addContainer :: ContainerType -> ContainerM ()
addContainer ct = modify $ \(ContainerStack top rest) ->
                      ContainerStack (Container ct mempty) (top:rest)

-- Step 2

-- Convert Document container and reference map into an AST.
processDocument :: (Container, ParseOptions) -> Blocks Text
processDocument (Container Document cs, opts) = processElts opts (toList cs)
processDocument _ = error "top level container is not Document"

-- Turn the result of `processLines` into a proper AST.
-- This requires grouping text lines into paragraphs
-- and list items into lists, handling blank lines,
-- parsing inline contents of texts and resolving referencess.
processElts :: ParseOptions -> [Elt] -> Blocks Text
processElts _ [] = mempty

processElts opts (L _lineNumber lf : rest) =
  case lf of
    -- Gobble text lines and make them into a Para:
    TextLine t ->
      singleton (Para $ parseInlines opts txt) <> processElts opts rest'
        where txt = T.stripEnd $ T.joinLines $ map T.stripStart
                               $ t : map extractText textlines
              (textlines, rest') = span isTextLine rest

    -- Blanks at outer level are ignored:
    BlankLine{} -> processElts opts rest

    -- Headers:
    ATXHeader lvl t -> Header lvl (parseInlines opts t) <| processElts opts rest
    SetextHeader lvl t -> Header lvl (parseInlines opts t) <| processElts opts rest
    SetextToken _ _ -> error "Setext token wasn't converted to setext header"

    -- Horizontal rule:
    Rule -> HRule <| processElts opts rest

processElts opts (C (Container ct cs) : rest) =
  case ct of
    Document -> error "Document container found inside Document"

    BlockQuote -> Quote (processElts opts (toList cs)) <| processElts opts rest

    -- List item?  Gobble up following list items of the same type
    -- (skipping blank lines), determine whether the list is tight or
    -- loose, and generate a List.
    ListItem { itemType = itemType } ->
      List itemType isTight items' <| processElts opts rest'
        where
          xs = takeListItems rest

          rest' = drop (length xs) rest

          -- take list items as long as list type matches and we
          -- don't hit two blank lines:
          takeListItems (c@(C (Container ListItem { itemType = lt } _)) : zs)
            | listTypesMatch lt itemType = c : takeListItems zs
          takeListItems (lf@(L _ (BlankLine _)) : c@(C (Container ListItem { itemType = lt } _)) : zs)
            | listTypesMatch lt itemType = lf : c : takeListItems zs
          takeListItems _ = []

          listTypesMatch (Bullet c1) (Bullet c2) = c1 == c2
          listTypesMatch (Ordered w1 _) (Ordered w2 _) = w1 == w2
          listTypesMatch _ _ = False

          items = mapMaybe getItem (Container ct cs : [c | C c <- xs])

          getItem (Container ListItem{} cs') = Just $ toList cs'
          getItem _                          = Nothing

          items' = map (processElts opts) items

          isTight = not (any isBlankLine xs) && all tightListItem items

          tightListItem []  = True
          tightListItem [_] = True
          tightListItem (_:is) = not $ any isBlankLine $ init is

    FencedCode _ _ info -> CodeBlock (parseInfoString <$> info)
                                     (T.unlines $ map extractText $ toList cs)
                                <| processElts opts rest

    IndentedCode -> CodeBlock Nothing txt <| processElts opts rest'
        where txt = T.unlines $ stripTrailingEmpties $ concatMap extractCode cbs
              stripTrailingEmpties = reverse . dropWhile (T.all (== ' ')) . reverse

              -- explanation for next line:  when we parsed
              -- the blank line, we dropped 0-3 spaces.
              -- but for this, code block context, we want
              -- to have dropped 4 spaces. we simply drop
              -- one more:
              extractCode (L _ (BlankLine t)) = [T.drop 1 t]
              extractCode (C (Container IndentedCode cs')) =
                  map extractText $ toList cs'
              extractCode _ = []

              (cbs, rest') = span (isIndentedCode <||> isBlankLine)
                                  (C (Container ct cs) : rest)

    RawHtmlBlock -> HtmlBlock txt <| processElts opts rest
        where txt = T.unlines (map extractText (toList cs))

    -- References have already been taken into account in the reference map,
    -- so we just skip.
    Reference -> processElts opts rest

extractText :: Elt -> Text
extractText (L _ (TextLine t)) = t
extractText _ = mempty

-- Step 1

processLines :: Text -> (Container, ReferenceMap)
processLines t = evalRWS (mapM_ processLine lns >> closeStack) () initState
  where
    lns = zip [1..] $ T.lines' $ T.replace "\0" "\xFFFD" t
    initState = ContainerStack (Container Document mempty) []

-- The main block-parsing function.
-- We analyze a line of text and modify the container stack accordingly,
-- adding a new leaf, or closing or opening containers.
processLine :: (LineNumber, Text) -> ContainerM ()
processLine (lineNumber, txt) = do
  ContainerStack top@(Container ct cs) rest <- get
  -- Apply the line-start scanners appropriate for each nested container.
  -- Return the remainder of the string, and the number of unmatched
  -- containers.
  let (t', numUnmatched) = tryOpenContainers (reverse $ top:rest) txt

  -- Some new containers can be started only after a blank.
  let lastLineIsText = numUnmatched == 0 &&
                       case viewr cs of
                            (_ :> L _ (TextLine _)) -> True
                            _                       -> False

  -- Process the rest of the line in a way that makes sense given
  -- the container type at the top of the stack (ct):
  case ct of
    -- If it's a verbatim line container, add the line.
    RawHtmlBlock{} | numUnmatched == 0 -> addLeaf lineNumber (TextLine t')
    IndentedCode   | numUnmatched == 0 -> addLeaf lineNumber (TextLine t')
    FencedCode{ fence = fence' } | numUnmatched == 0 ->
      if isRight $ parse scanClosing t'
         -- closing code fence
         then closeContainer
         else addLeaf lineNumber (TextLine t')
        where scanClosing = upToCountChars 3 (== ' ')
                          >> string fence' >> scanChars (T.head fence')
                                           >> scanSpaces >> endOfInput


    -- otherwise, parse the remainder to see if we have new container starts:
    _ -> case tryNewContainers (isListItem ct) lastLineIsText (T.length txt - T.length t') t' of

       -- lazy continuation: text line, last line was text, no new containers,
       -- some unmatched containers:
       ([], TextLine t)
           | numUnmatched > 0
           , (_ :> L _ (TextLine _)) <- viewr cs
           , ct /= IndentedCode
           -> addLeaf lineNumber (TextLine t)

       -- A special case: Lazy continuation of a list item looking like
       -- indented code e.g:
       -- "  1.  Paragraph"
       -- "    with two lines"
       (IndentedCode : _, TextLine t)
           | numUnmatched > 0
           , (_ :> L _ (TextLine _)) <- viewr cs
           , ListItem _ _ <- ct
           -> addLeaf lineNumber (TextLine (T.strip t))

       -- if it's a setext header line and the top container has a textline
       -- as last child, add a setext header:
       ([], SetextToken lev setextText) | numUnmatched == 0 ->
           case viewr cs of
             ((viewr -> _ :> L _ (TextLine _)) :> L _ (TextLine _)) ->
               addLeaf lineNumber $ if isRight $ parse scanHRuleLine setextText
                                    then Rule
                                    else TextLine setextText
             (cs' :> L _ (TextLine t)) -> -- replace last text line with setext header
               put $ ContainerStack (Container ct
                        (cs' |> L lineNumber (SetextHeader lev $ T.strip t))) rest
               -- Note: the following case should not occur, since
               -- we don't add a SetextHeader leaf unless lastLineIsText.
             _ -> error "setext header line without preceding text line"

       -- otherwise, close all the unmatched containers, add the new
       -- containers, and finally add the new leaf:
       (ns, lf) -> do -- close unmatched containers, add new ones
           _ <- replicateM numUnmatched closeContainer
           _ <- mapM_ addContainer ns
           case (reverse ns, lf) of
             -- don't add extra blank at beginning of fenced code block
             (FencedCode{}:_,  BlankLine{}) -> return ()
             _ -> addLeaf lineNumber lf

-- Try to match the scanners corresponding to any currently open containers.
-- Return remaining text after matching scanners, plus the number of open
-- containers whose scanners did not match.  (These will be closed unless
-- we have a lazy text line.)
tryOpenContainers :: [Container] -> Text -> (Text, Int)
tryOpenContainers cs t = case parse (scanners $ map containerContinue cs) t of
                         Right (t', n) -> (t', n)
                         Left e -> error $ "error parsing scanners: " ++ show e
  where scanners [] = (,0) <$> takeText
        scanners (p:ps) = (p *> scanners ps)
                       <|> ((,length (p:ps)) <$> takeText)

-- Try to match parsers for new containers.  Return list of new
-- container types, and the leaf to add inside the new containers.
tryNewContainers :: Bool -> Bool -> Int -> Text -> ([ContainerType], Leaf)
tryNewContainers afterListItem lastLineIsText offset t =
  case parse newContainers t of
       Right (cs,t') -> (cs, t')
       Left err      -> error (show err)
  where newContainers = do
          getPosition >>= \pos -> setPosition pos{ column = offset + 1 }
          regContainers <- many (containerStart afterListItem lastLineIsText)
          verbatimContainers <- option []
                            $ count 1 (verbatimContainerStart lastLineIsText)
          if null verbatimContainers
             then (regContainers,) <$> leaf lastLineIsText
             else (regContainers ++ verbatimContainers,) <$> textLineOrBlank

textLineOrBlank :: Parser Leaf
textLineOrBlank = consolidate <$> takeText
  where consolidate ts | T.all (==' ') ts = BlankLine ts
                       | otherwise        = TextLine  ts

-- Parse a leaf node.
leaf :: Bool -> Parser Leaf
leaf lastLineIsText = scanNonindentSpace *> choice
    [ ATXHeader <$> parseAtxHeaderStart <*> parseAtxHeaderContent
    , guard lastLineIsText *> parseSetextToken
    , Rule <$ scanHRuleLine
    , textLineOrBlank
    ]

-- Scanners

scanReference :: Scanner
scanReference = discard $ lookAhead (char '[')

-- Scan the beginning of a blockquote:  up to three spaces
-- indent (outside of this scanner), the `>` character, and an optional space.
scanBlockquoteStart :: Scanner
scanBlockquoteStart = scanChar '>' >> option () (scanChar ' ')

-- Parse the sequence of `#` characters that begins an ATX
-- header, and return the number of characters.  We require
-- a space after the initial string of `#`s, as not all markdown
-- implementations do. This is because (a) the ATX reference
-- implementation requires a space, and (b) since we're allowing
-- headers without preceding blank lines, requiring the space
-- avoids accidentally capturing a line like `#8 toggle bolt` as
-- a header.
parseAtxHeaderStart :: Parser Int
parseAtxHeaderStart = do
  _ <- char '#'
  hashes <- upToCountChars 5 (== '#')
  -- hashes must be followed by space unless empty header:
  notFollowedBy (skip (/= ' '))
  return $ T.length hashes + 1

parseAtxHeaderContent :: Parser Text
parseAtxHeaderContent = T.strip . removeATXSuffix <$> takeText
  where removeATXSuffix t =
          case dropTrailingHashes of
                 t' | T.null t' -> t'
                      -- an escaped \#
                    | T.last t' == '\\' -> t' <> T.replicate trailingHashes "#"
                    | T.last t' /= ' ' -> t
                    | otherwise -> t'
          where dropTrailingSpaces = T.dropWhileEnd (== ' ') t
                dropTrailingHashes = T.dropWhileEnd (== '#') dropTrailingSpaces
                trailingHashes = T.length dropTrailingSpaces - T.length dropTrailingHashes

parseSetextToken :: Parser Leaf
parseSetextToken = fmap (uncurry SetextToken) $ withConsumed $ do
  d <- satisfy (\c -> c == '-' || c == '=')
  skipWhile (== d)
  scanBlankline
  return $ if d == '=' then 1 else 2

-- Scan a horizontal rule line: "...three or more hyphens, asterisks,
-- or underscores on a line by themselves. If you wish, you may use
-- spaces between the hyphens or asterisks."
scanHRuleLine :: Scanner
scanHRuleLine = do
  c <- satisfy (\c -> c == '*' || c == '_' || c == '-')
  _ <- count 2 $ scanSpaces >> skip (== c)
  skipWhile (\x -> x == ' ' || x == c)
  endOfInput

-- Parse an initial code fence line, returning
-- the fence part and the rest (after any spaces).
parseCodeFence :: Parser ContainerType
parseCodeFence = do
  col <- column <$> getPosition
  cs <- takeWhile1 (=='`') <|> takeWhile1 (=='~')
  guard $ T.length cs >= 3
  scanSpaces
  rawattr <- optional (takeWhile1 (\c -> c /= '`' && c /= '~'))
  endOfInput
  return FencedCode { startColumn = col
                    , fence = cs
                    , info = rawattr }

-- Scan the start of an HTML block:  either an HTML tag or an
-- HTML comment, with no indentation.
scanHtmlBlockStart :: Scanner
scanHtmlBlockStart = discard $ lookAhead $ scanNonindentSpace *> choice
    [ isBlockHtmlTag `mfilter` incompleteTag
    , "<!--", "<?", "<!", "<![CDATA["]
    where tagName = T.cons <$> satisfy (inClass "A-Za-z")
                           <*> takeWhile (inClass "A-Za-z0-9")
          incompleteTag = char '<' *> discardOpt (char '/') *> tagName

-- List of block level tags for HTML 5.
isBlockHtmlTag :: Text -> Bool
isBlockHtmlTag name = T.toLower name `Set.member` Set.fromList
    ["article","header","aside","hgroup","blockquote","hr","iframe","body"
    ,"li","map","button","object","canvas","ol","caption","output","col","p"
    ,"colgroup","pre","dd","progress","div","section","dl","table","td","dt"
    ,"tbody","embed","textarea","fieldset","tfoot","figcaption","th","figure"
    ,"thead","footer","tr","form","ul","h1","h2","h3","h4","h5","h6","video"
    ,"script","style"]

-- Parse a list marker and return the list type.
--
-- "  1.  Content"
--  ^^  ^^-- contentPadding
--  \\-- markerPadding
parseListMarker :: Bool -> Parser ContainerType
parseListMarker afterListItem = do
  markerPadding <- if afterListItem then countSpaces else countNonindentSpace
  ty <- parseBullet <|> parseListNumber
  -- padding is 1 if list marker followed by a blank line
  -- or indented code.  otherwise it's the length of the
  -- whitespace between the list marker and the following text:
  contentPadding <- (1 <$ scanBlankline)
                <|> (1 <$ (skip (==' ') *> lookAhead (count 4 (char ' '))))
                <|> (T.length <$> takeWhile (==' '))
  -- text can't immediately follow the list marker:
  guard $ contentPadding > 0
  return ListItem { itemType = ty
                  , padding = markerPadding + contentPadding + listMarkerWidth ty
                  }

listMarkerWidth :: ListType -> Int
listMarkerWidth (Bullet _) = 1
listMarkerWidth (Ordered _ n)
    | n < 10    = 2
    | n < 100   = 3
    | n < 1000  = 4
    | otherwise = 5

-- Parse a bullet and return list type.
parseBullet :: Parser ListType
parseBullet = do
  c <- satisfy (\c -> c == '+' || c == '*' || c == '-')
  unless (c == '+') $
      nfb (count 2 (scanSpaces >> skip (== c)) >>
             skipWhile (\x -> x == ' ' || x == c) >> endOfInput) -- hrule
  return $ Bullet $ bulletChar c
  where bulletChar '+' = Plus
        bulletChar '-' = Minus
        bulletChar '*' = Asterisk
        bulletChar _   = error "Shouldn't happen"

-- Parse a list number marker and return list type.
parseListNumber :: Parser ListType
parseListNumber = do
    num  <- decimal
    wrap <- choice [Period <$ scanChar '.', Paren <$ scanChar ')']
    return $ Ordered wrap num
