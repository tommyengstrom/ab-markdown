-- | Render the AST as Markdown

module Comark.Render where

import           Comark.Syntax
import qualified Data.Text                                    as T
import           Data.Text                      ( Text )
import           Data.Sequence                  ( Seq )
import           Data.Foldable

renderDoc :: Doc Text -> Text
renderDoc (Doc bs) = renderBlocks bs

renderBlocks :: Foldable f => f (Block Text) -> Text
renderBlocks = foldMap renderBlock

renderBlock :: Block Text -> Text
renderBlock = \case
    ThematicBreak               -> "\n---\n"
    Heading   hl    is          -> renderHeadingLevel hl <> renderInlines is
    CodeBlock mLang t           -> renderCodeBlock mLang t
    Paragraph is                -> renderParagraph is
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
renderInlines = undefined

renderCodeBlock :: Maybe Language -> Text -> Text
renderCodeBlock ml t = mconcat ["```", lang, "\n", t, "```"]
  where
    lang :: Text
    lang = case ml of
        Just Haskell     -> "haskell"
        Just (Unknown l) -> l
        Nothing          -> ""

renderParagraph :: Inlines Text -> Text
renderParagraph is = "\n" <> renderInlines is

renderQuestion :: Blocks Text -> Maybe (Blocks Text) -> Text
renderQuestion qBlocks mAnsBlocks = "\n" <> question <> answer
  where
    question = T.replace "\n" "\n?? " (renderBlocks qBlocks)
    answer   = case mAnsBlocks of
        Nothing -> ""
        Just bs -> T.replace "\n" "\n?= " (renderBlocks bs)


renderQuote :: Blocks Text -> Text
renderQuote = undefined

renderList :: ListType -> Bool -> (Seq (Blocks Text)) -> Text
renderList lt tight blocksSeq = T.intercalate "\n" . zipWith mkBlock [1 ..] $ toList
    blocksSeq
  where
    mkBlock :: Int -> Blocks Text -> Text
    mkBlock i bs = marker i <> " " <> renderBlocks bs

    marker :: Int -> Text
    marker i = case lt of
        Ordered Period start -> "- " <> T.pack (show $ start + i) <> ". "
        Ordered Paren  start -> "- (" <> T.pack (show $ start + i) <> ") "
        Bullet Minus         -> "- "
        Bullet Plus          -> "+ "
        Bullet Asterisk      -> "* "
