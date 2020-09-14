module AbMarkdown.Parser.Inline
  ( parseInlines
  , pReference
  , parseLanguage
  ) where

import Prelude hiding (takeWhile)

import Control.Applicative
import Control.Bool
import Control.Monad       hiding (mapM_)

import           Data.Char.Extended
import           Data.Foldable          (asum)
import           Data.List              (foldl')
import           Data.Maybe
import           Data.Sequence
  (ViewL(..), singleton, viewl, (<|), (|>))
import qualified Data.Sequence.Extended as Seq
import           Data.Text              (Text)
import qualified Data.Text              as Text
import AbMarkdown.Parser.Inline.EmphLink
import AbMarkdown.Parser.Options         (ParserOptions(..))
import AbMarkdown.Parser.Reference
import AbMarkdown.Parser.Util
import AbMarkdown.ParserCombinators
import AbMarkdown.Syntax
import Text.Html.Email.Validate
import Text.Html.Entity

parseInlines :: ParserOptions -> Text -> Inlines ()
parseInlines opts input =
  case runParser (pInlines opts <* endOfInput) input of
    Left e ->
      error $ "[INTERNAL ERROR]: parseInlines: " <> show e
    Right r
      | _poNormalize opts -> normalize r
      | otherwise -> r

pInlines :: ParserOptions -> Parser (Inlines ())
pInlines = fmap msum . many . pInline

pInline :: ParserOptions -> Parser (Inlines ())
pInline opts =
  asum
    [ pText
    , pHardbreak
    , pSoftbreak
    , pTask opts
    , guard (_poParseEmphasis opts) *> pEmphLink opts
    , pBackslashed
    , pAutolink
    , pCode
    , pEntity
    , pFallback
    ]

parseLanguage :: Text -> Language
parseLanguage t =
  case runParser (msum <$> many parser <* endOfInput) t of
    Left _   -> Unknown t
    Right is -> case Text.toLower $ foldMap asText is of
        "haskell" -> Haskell
        s -> Unknown s
  where
    parser = pText <|> pBackslashed <|> pEntity

str :: Text -> Inlines ()
str = singleton . Str

pText :: Parser (Inlines ())
pText = str <$> takeWhile1 (not . isSpecial)

pFallback :: Parser (Inlines ())
pFallback = str <$> (Text.singleton <$> satisfy isSpecial)

isSpecial :: Char -> Bool
isSpecial = inClass "\\`*_[]!&<\t\n\r "

-- | Either backslash-escaped punctuation or an actual backslash
pBackslashedChar :: Parser Text
pBackslashedChar =
  Text.singleton <$> (char '\\' *> option '\\' (satisfy isAsciiPunctuation))

-- Parses a (possibly escaped) character satisfying the predicate.
pSatisfy :: (Char -> Bool) -> Parser Char
pSatisfy p = asum
  [ satisfy ((/= '\\') <&&> p)
  , char '\\' *> satisfy (isAsciiPunctuation <&&> p)
  , guard (p '\\') *> char '\\'
  ]

pBackslashed :: Parser (Inlines ())
pBackslashed = str <$> pBackslashedChar

pHardbreak :: Parser (Inlines ())
pHardbreak =
  singleton HardBreak
    <$ asum [ void (char '\\'), spaceScape ] <* pLineEnding
    <* skipWhile (== ' ') -- ignore next line's leading spaces
  where
    spaceScape = do
      _ <- replicateM 2 (char ' ')  -- two spaces
      skipWhile (== ' ')       -- and more spaces (optionally)

pSoftbreak :: Parser (Inlines ())
pSoftbreak =
  discardOpt (char ' ')
    *> pLineEnding
    *> pure (singleton SoftBreak)
    <* skipWhile (== ' ')

-- [ Code ] --------------------------------------------------------------------

pCode :: Parser (Inlines ())
pCode = singleton <$> do
  startTicks <- backtickChunk
  let pEndTicks = string startTicks <* notFollowedBy (char '`')
      pContent  = code <$> (codechunk `manyTill` pEndTicks)
      fallback  = Str startTicks
  option fallback pContent
  where
    code      = Code . Text.strip . Text.concat
    codechunk = backtickChunk <|> nonBacktickChunk <|> spaceChunk

    backtickChunk      = takeWhile1 (== '`')
    nonBacktickChunk   = takeWhile1 ((/= '`') <&&> (not . isCollapsableSpace))
    spaceChunk         =  " " <$ takeWhile1 isCollapsableSpace
    isCollapsableSpace = (== ' ') <||> isLineEnding


pTask :: ParserOptions -> Parser (Inlines ())
pTask opts = fmap singleton $ do
    _ <- char '['
    status <- const Done <$> char 'x'
          <|> const Todo <$> char ' '
    _ <- char ']'
    _ <- char ' '
    Task () status <$> pInlines opts

-- [ Entities ] ----------------------------------------------------------------

pEntity :: Parser (Inlines ())
pEntity = str <$> pEntityText

pEntityText :: Parser Text
pEntityText = char '&' *> entityBody <* char ';'
  where
    entityBody =
      codepointEntity <|> namedEntity
    codepointEntity = do
      _ <- char '#'
      decEntity <|> hexEntity
    namedEntity = do
      name <- takeWhile1 (/= ';')
      -- asum works over Maybe. Nothing turns into mzero.
      asum (pure <$> entityNameChars name)
        <?> "not a named entity"
    decEntity =
      Text.singleton . chrSafe <$> decimal
    hexEntity = do
      _ <- char 'x' <|> char 'X'
      Text.singleton . chrSafe <$> hexadecimal

-- [ Autolinks ] ---------------------------------------------------------------
pAutolink :: Parser (Inlines ())
pAutolink = char '<' *> (pUrl <|> pEmail) <* char '>'
    where pUrl = do
              scheme <- pScheme
              _ <- char ':'
              chars <- takeTill ((isAscii <&&> (isWhitespace <||> isControl))
                                    <||> (== '>') <||> (== '<'))
              let uri = scheme <> ":" <> chars
              pure $ singleton $ Link (str uri) (LinkRef uri) Nothing
          pEmail = do
              email <- isValidEmail `mfilter` takeWhile1 (/= '>')
              pure $ singleton $
                Link (str email) (LinkRef $ "mailto:" <> email) Nothing

pScheme :: Parser Text
pScheme = do
  a <- satisfy (isAscii <&&> isLetter)
  as <- takeWhile1 ((isAscii <&&> (isLetter <||> isDigit)) <||> (== '+') <||> (== '.') <||> (== '-'))
  mfilter ((<= 32) . Text.length) $ pure $ Text.cons a as

-- [ Emphasis, Links, and Images ] ---------------------------------------------

pEmphDelimToken :: EmphIndicator -> Parser Token
pEmphDelimToken indicator@(indicatorChar -> c) = do
  preceded <- peekLastChar
  delim <- takeWhile1 (== c)
  followed <- peekChar
  let isLeft  = check followed preceded
      isRight = check preceded followed
      precededByPunctuation = fromMaybe False (isPunctuation <$> preceded)
      followedByPunctuation = fromMaybe False (isPunctuation <$> followed)
      canOpen =
        isLeft && (isAsterisk indicator || not isRight || precededByPunctuation)
      canClose =
        isRight && (isAsterisk indicator || not isLeft || followedByPunctuation)

  pure $ EmphDelimToken $ EmphDelim indicator (Text.length delim) canOpen canClose
  where
    check :: Maybe Char -> Maybe Char -> Bool
    check a b =
      and
        [ fromMaybe False (not . isUnicodeWhitespace <$> a)
        , or $ map (fromMaybe True)
            [ not . isPunctuation <$> a
            , (isUnicodeWhitespace <||> isPunctuation) <$> b
            ]
        ]

pLinkOpener :: Parser Token
pLinkOpener = do
  openerType <- option LinkOpener (ImageOpener <$ char '!')
  lbl <- optional (lookAhead pLinkLabel)
  _ <- char '['
  pure $ LinkOpenToken $ LinkOpen openerType True lbl mempty

pEmphLinkDelim :: Parser Token
pEmphLinkDelim = asum
  [ pEmphDelimToken AsteriskIndicator
  , pEmphDelimToken UnderscoreIndicator
  , pLinkOpener
  ]

pEmphTokens :: ParserOptions -> Parser DelimStack
pEmphTokens opts = do
  delim <- pEmphLinkDelim
  foldP step (Seq.singleton delim)
  where
    step ds = asum
      [ Just <$> asum
          [ char ']' *> lookForLinkOrImage ds
          , (ds |>) <$> pEmphLinkDelim
          , addInlines ds <$> pInline opts { _poParseEmphasis = False }
          ]
      , Nothing <$ endOfInput
      ]

    lookForLinkOrImage :: DelimStack -> Parser DelimStack
    lookForLinkOrImage ds =
      case Seq.findr isLinkOpener ds of
        Nothing -> pure (addInline ds closer)
        Just (suffix, LinkOpenToken opener, prefix) ->
          option fallback $ do
            guard (linkActive opener)
            addInlines (deactivating prefix) <$> pLink
          where
            fallback =
              addInlines prefix (unLinkOpen opener) <> addInline suffix closer
            constr (runLinkDestination -> d) (fmap runLinkTitle -> t) =
              case linkOpenerType opener of
                LinkOpener  -> Link content (LinkRef d) t
                ImageOpener -> Image content (LinkRef d) t
            deactivating =
              case linkOpenerType opener of
                LinkOpener  -> fmap deactivate
                ImageOpener -> id
            content =
              foldMap unToken
                $ processEmphTokens
                $ InlineToken (linkContent opener) <| suffix
            pLink =
              pInlineLink constr <|> pReferenceLink constr (linkLabel opener)
        Just (_, _, _) ->
          error "lookForLinkOrImage: impossible happened. expected LinkOpenToken"
      where
        closer = Str "]"

    pInlineLink constr = do
      _ <- char '(' *> optional pWhitespace
      dest <- option "" pLinkDest
      title <- optional (pWhitespace *> pLinkTitle <* optional pWhitespace)
      _ <- char ')'
      pure $ singleton $ constr dest title
    pReferenceLink constr lbl = do
      Just ref <- (Just <$> pLinkLabel) <|> (lbl <$ optional "[]")
      Just link <- pure $ _poLinkReferences opts ref
      pure $ singleton $ uncurry constr link

pEmphLink :: ParserOptions -> Parser (Inlines ())
pEmphLink opts =
  foldMap unToken . processEmphTokens <$> pEmphTokens opts

processEmphTokens :: DelimStack -> DelimStack
processEmphTokens = foldl' processEmphToken Seq.empty

processEmphToken :: DelimStack -> Token -> DelimStack
processEmphToken stack token =
  case token of
    InlineToken{} ->
      stack |> token
    LinkOpenToken{} ->
      processEmphToken stack (InlineToken $ unToken token)
    EmphDelimToken closing
      | emphCanOpen closing && not (emphCanClose closing) ->
          stack |> EmphDelimToken closing
      | emphCanClose closing ->
      case Seq.findr (matchOpening (emphIndicator closing)) stack of
        Nothing
          | emphCanClose closing ->
              stack |> EmphDelimToken closing
          | otherwise ->
              stack |> InlineToken (unEmphDelim closing)
        Just (viewl -> EmptyL, _, _) -> stack
        Just (content, EmphDelimToken opening, rest)
          | emphCanOpen closing && ((emphLength opening + emphLength closing) `mod` 3) == 0 ->
              stack |> EmphDelimToken closing
          | otherwise ->
              matchEmphStrings rest opening closing
                (foldMap unToken content)
        Just (_, _, _) ->
          error "processEmphToken: Impossible happened. Expected EmphDelimToken"
      | otherwise ->
         stack |> InlineToken (unEmphDelim closing)
  where
    matchOpening ch (EmphDelimToken d) = emphIndicator d == ch && emphCanOpen d
    matchOpening _ _ = False

matchEmphStrings :: DelimStack -> EmphDelim -> EmphDelim -> Inlines () -> DelimStack
matchEmphStrings stack opening closing content
  | emphIndicator opening == emphIndicator closing = if
     | emphLength closing == emphLength opening ->
         stack
           |> InlineToken (emph (emphLength closing) content)
     | emphLength closing < emphLength opening ->
         stack
           |> EmphDelimToken opening
                { emphLength = emphLength opening - emphLength closing }

           |> InlineToken (emph (emphLength closing) content)
     | emphLength closing > emphLength opening ->
          processEmphToken
            (stack |> InlineToken (emph (emphLength opening) content))
            (EmphDelimToken closing
               { emphLength = emphLength closing - emphLength opening})
     | otherwise -> stack
  | otherwise = stack

emph :: Int -> Inlines () -> Inlines ()
emph n content
  | n <= 0    = content
  | even n    = single Strong $ emph (n - 2) content
  | otherwise = single Emph   $ emph (n - 1) content
  where
   single f is =
     f is <$ guard (not $ Seq.null is)

pLinkLabel :: Parser LinkText
pLinkLabel =
  char '[' *> (LinkText . Text.concat <$> someTill chunk (char ']'))
  where
    chunk          = regChunk <|> bracketChunk <|> backslashChunk
    regChunk       = takeWhile1 (`notElem` ("[]\\" :: [Char]))
    bracketChunk   = char '\\' *> ("[" <|> "]")
    backslashChunk = "\\\\"

pLinkDest :: Parser LinkDestination
pLinkDest = LinkDestination <$> (pointy <|> nonPointy)
  where
    pointy = char '<' *> (Text.concat <$> many chunk) <* char '>'
      where
        chunk = asum
          [ takeWhile1 (`notElem` [' ', '\r', '\n', '>', '<', '\\'])
          , Text.singleton <$> pSatisfy (`notElem` [' ', '\r', '\n', '>', '<'])
          ]
    nonPointy = Text.concat <$> some chunk
      where
        chunk = asum
          [ takeWhile1 (notInClass " ()\\&" <&&> not . isControl)
          , pEntityText
          , pBackslashedChar
          , parenthesize . Text.concat <$> do
              _ <- char '('
              manyTill chunk (char ')')
          ]

pLinkTitle :: Parser LinkTitle
pLinkTitle = LinkTitle <$> surroundedWith ("('\"'" :: [Char])
  where
    surroundedWith openers = do
      opener <- satisfy (`elem` openers)
      let ender = if opener == '(' then ')' else opener
          pEnder = char ender <* notFollowedBy (skip isAlphaNum)
          regChunk = asum
            [ takeWhile1 ((/= ender) <&&> (/= '\\') <&&> (/= '&'))
            , pEntityText
            , "&"
            , pBackslashedChar
            ]
          nestedChunk = parenthesize <$> surroundedWith "("
      Text.concat <$> manyTill (regChunk <|> nestedChunk) pEnder

pReference :: Parser (LinkText, LinkDestination, Maybe LinkTitle)
pReference = do
  lab <- pLinkLabel <* char ':'
  guard $ isJust $ Text.find (not . isWhitespace) $ runLinkText lab
  scanWhitespaceNL
  url <- pLinkDest <* skipWhile whitespaceNoNL
  titleOnNewLine <- isJust <$> optional pLineEnding
  skipWhile whitespaceNoNL
  title <-
    if titleOnNewLine
    then optional $
           pLinkTitle <* do
             skipWhile whitespaceNoNL
             endOfInput <|> void pLineEnding
    else optional pLinkTitle <* do
           skipWhile whitespaceNoNL
           endOfInput <|> void pLineEnding
  pure (lab, url, title)
  where
    -- | optional scanWhitespace (including up to one line ending)
    scanWhitespaceNL = do
      skipWhile whitespaceNoNL
      _ <- optional pLineEnding
      skipWhile whitespaceNoNL
    whitespaceNoNL =
      isWhitespace <&&> not . isLineEnding

