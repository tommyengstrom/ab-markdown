{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           AbMarkdown.Parser
import           AbMarkdown.Render
import           AbMarkdown.Syntax              ( )
import           Test.QuickCheck
import           Data.Text                      ( Text )
import qualified Data.Text                                    as T

instance Arbitrary Text where
    arbitrary = T.intercalate " " <$> listOf bogusWord
      where
        bogusWord :: Gen Text
        bogusWord = fmap T.pack . listOf1 $ elements ['a' .. 'z']


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
        prop "Full document works" $ do
            doc <- arbitrary
            pure $ doc `shouldBe` parse [] (render doc)
