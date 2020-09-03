-- | Render the AST as Markdown

module AbMarkdown.Render where

import           AbMarkdown.Syntax
import qualified Data.Text                                    as T
import           Data.Text                      ( Text )
import           Data.Sequence                  ( Seq )
import           Data.Foldable

render :: Doc Text -> Text
render (Doc bs) = T.strip $ renderBlocks bs

renderBlocks :: Foldable f => f (Block Text) -> Text
renderBlocks = foldMap renderBlock

renderBlock :: Block Text -> Text
renderBlock = \case
    ThematicBreak               -> "---\n\n"
    Heading   hl    is          -> renderHeadingLevel hl <> renderInlines is <> "\n\n"
    CodeBlock mLang t           -> renderCodeBlock mLang t
    Paragraph is                -> renderInlines is <> "\n\n"
    Question qBlocks mAnsBlocks -> renderQuestion qBlocks mAnsBlocks
    Quote bs                    -> renderQuote bs
    List lt tight bs            -> renderList lt tight bs

renderHeadingLevel :: HeadingLevel -> Text
renderHeadingLevel = \case
    Heading1 -> "# "
    Heading2 -> "## "
    Heading3 -> "### "
    Heading4 -> "#### "
    Heading5 -> "##### "
    Heading6 -> "###### "

renderInlines :: Inlines Text -> Text
renderInlines = foldMap renderInline

renderInline :: Inline Text -> Text
renderInline = \case
    Str    t             -> t
    Emph   is            -> "*" <> renderInlines is <> "* "
    Strong is            -> "**" <> renderInlines is <> "** "
    Code   t             -> "`" <> t <> "`"
    Link  is dest _title -> "[" <> renderInlines is <> "](" <> unLinkRef dest <> ")"
    Image is dest _title -> "![" <> renderInlines is <> "](" <> unLinkRef dest <> ")"
    SoftBreak            -> "\n"
    HardBreak            -> "\\\n"
    Task status is       -> case status of
        Todo -> "[ ] " <> renderInlines is
        Done -> "[x] " <> renderInlines is


renderCodeBlock :: Maybe Language -> Text -> Text
renderCodeBlock ml t = mconcat ["```", lang, "\n", t, "```"]
  where
    lang :: Text
    lang = case ml of
        Just Haskell     -> "haskell"
        Just (Unknown l) -> l
        Nothing          -> ""

renderQuestion :: Blocks Text -> Maybe (Blocks Text) -> Text
renderQuestion qBlocks mAnsBlocks = "\n" <> question <> answer
  where
    question = T.unlines . fmap ("?? " <>) . T.lines $ renderBlocks qBlocks
    answer   = case mAnsBlocks of
        Nothing -> ""
        Just bs -> T.unlines . fmap ("?= " <>) . T.lines $ renderBlocks bs


renderQuote :: Blocks Text -> Text
renderQuote = T.unlines . fmap ("> " <>) . T.lines . renderBlocks

renderList :: ListType -> Bool -> (Seq (Blocks Text)) -> Text
renderList lt _tight blocksSeq = T.intercalate "\n" . zipWith mkBlock [0 ..] $ toList
    blocksSeq
  where
    mkBlock :: Int -> Blocks Text -> Text
    mkBlock i bs = marker i <> " " <> T.strip (renderBlocks bs)

    marker :: Int -> Text
    marker i = case lt of
        Ordered Period start -> T.pack (show $ start + i) <> ". "
        Ordered Paren  start -> T.pack (show $ start + i) <> ") "
        Bullet Minus         -> "- "
        Bullet Plus          -> "+ "
        Bullet Asterisk      -> "* "
