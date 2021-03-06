-- | Render the AST as Markdown

module AbMarkdown.Render where

import           AbMarkdown.Syntax
import qualified Data.Text                                    as T
import           Data.Text                      ( Text )
import           Data.Sequence                  ( Seq )
import           Data.Foldable

render :: Doc id -> Text
render (Doc bs) = T.strip $ renderBlocks bs

renderBlocks :: Foldable f => f (Block id) -> Text
renderBlocks = foldMap ((<> "\n\n") . renderBlock)

renderBlock :: Block id -> Text
renderBlock = \case
    ThematicBreak                   -> "---"
    Heading   hl    is              -> renderHeadingLevel hl <> renderInlines is
    CodeBlock mLang t               -> renderCodeBlock mLang t
    Paragraph is                    -> renderInlines is
    Question _id qBlocks mAnsBlocks -> renderQuestion qBlocks mAnsBlocks
    Quote bs                        -> renderQuote bs
    List lt tight bs                -> renderList lt tight bs

renderHeadingLevel :: HeadingLevel -> Text
renderHeadingLevel = \case
    Heading1 -> "# "
    Heading2 -> "## "
    Heading3 -> "### "
    Heading4 -> "#### "
    Heading5 -> "##### "
    Heading6 -> "###### "

renderInlines :: Inlines id -> Text
renderInlines = foldMap renderInline

renderInline :: Inline id -> Text
renderInline = \case
    Str    t             -> t
    Emph   is            -> "*" <> renderInlines is <> "* "
    Strong is            -> "**" <> renderInlines is <> "** "
    Code   t             -> "`" <> t <> "`"
    Link  is dest _title -> "[" <> renderInlines is <> "](" <> unLinkRef dest <> ")"
    Image is dest _title -> "![" <> renderInlines is <> "](" <> unLinkRef dest <> ")"
    SoftBreak            -> "\n"
    HardBreak            -> "\\\n"
    Task _id status is   -> case status of
        Todo -> "[ ] " <> renderInlines is
        Done -> "[x] " <> renderInlines is


renderCodeBlock :: Maybe Language -> Text -> Text
renderCodeBlock ml t = mconcat ["```", lang, "\n", T.strip t, "\n```"]
  where
    lang :: Text
    lang = case ml of
        Just Haskell     -> "haskell"
        Just (Unknown l) -> l
        Nothing          -> ""

renderQuestion :: Blocks id -> Maybe (Blocks id) -> Text
renderQuestion qBlocks mAns = question <> answer
  where
    question = prefixLines "?? " $ renderBlocks qBlocks
    answer   = prefixLines "?= " $ maybe "" renderBlocks mAns

    prefixLines :: Text -> Text -> Text
    prefixLines prefix = T.unlines . fmap (prefix <>) . T.lines . T.strip


renderQuote :: Blocks id -> Text
renderQuote = T.unlines . fmap ("> " <>) . T.lines . renderBlocks

renderList :: ListType -> Bool -> (Seq (Blocks id)) -> Text
renderList lt _tight blocksSeq = T.intercalate "\n" . zipWith mkBlock [0 ..] $ toList
    blocksSeq
  where
    mkBlock :: Int -> Blocks id -> Text
    mkBlock i bs = marker i <> " " <> T.strip (renderBlocks bs)
    marker :: Int -> Text
    marker i = case lt of
        Ordered Period start -> T.pack (show $ start + i) <> ". "
        Ordered Paren  start -> T.pack (show $ start + i) <> ") "
        Bullet Minus         -> "- "
        Bullet Plus          -> "+ "
        Bullet Asterisk      -> "* "
