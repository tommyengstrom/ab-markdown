module Main where

import           Test.Hspec
--import           Test.Hspec.QuickCheck
--import           AbMarkdown.Parser
--import           AbMarkdown.Render
--import           Test.QuickCheck


--arbitraryBlock :: Gen Text -> Gen (Block Text)
--arbitraryBlock genText = oneOf
--    [ ThematicBreak
--    , Heading <$> arbitrary (Inlines t)
--    , CodeBlock (Maybe Language) t
--    , Paragraph (Inlines t)
--    , Question (Blocks t) (Maybe (Blocks t))
--    , Quote (Blocks t) -- ^ Block Quote (a quoted sequence of blocks)
--    , List ListType Bool (Seq (Blocks t)) -- ^ List: Type of the list, tightness, a sequnce of blocks (list item)
--    ]

main :: IO ()
main = hspec $ do
    describe "AbMarkdown.Parser" $ do
        xit "Can parser question without answer" $ shouldBe False True
        xit "Can parser question with answer" $ shouldBe False True
    describe "Partial isomorphism `parse == parser . render . parse`" $ do
        xit "ThematicBreak block works" $ shouldBe False True
        xit "Heading block works" $ shouldBe False True
        xit "CodeBlock block works" $ shouldBe False True
        xit "Paragraph  block works" $ shouldBe False True
        xit "Question  block works" $ shouldBe False True
        xit "Quote  block works" $ shouldBe False True
        xit "List  block works" $ shouldBe False True
        xit "Full document works" $ shouldBe False True
