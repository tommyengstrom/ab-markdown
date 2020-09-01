{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           AbMarkdown.Parser
import           AbMarkdown.Render
import           AbMarkdown.Syntax
import           AbMarkdown.Elm                 ( )
import           Test.QuickCheck
import           Data.Text                      ( Text )
import qualified Data.Text                                    as T
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8                   as BL8
import           Debug.Trace
import           Control.Monad
-- import qualified Data.Sequence                                as Seq
-- import           Data.Aeson

instance Arbitrary Text where
    arbitrary = T.intercalate " " <$> listOf1 bogusWord
      where
        bogusWord :: Gen Text
        bogusWord = fmap T.pack . listOf1 $ elements ['a' .. 'z']

data InlineTest = InlineTest
    { asDoc    :: Doc Text
    , reparsed :: Doc Text
    , original :: Inline Text
    , rendered :: Text
    }
    deriving (Show, Eq)

main :: IO ()
main = hspec $ do
    describe "AbMarkdown.Parser" $ do
        xit "Can parser question without answer" $ shouldBe False True
        xit "Can parser question with answer" $ shouldBe False True
    describe "Partial isomorphism `parse == parser . render . parse`" $ do
        describe "Inline without recursion" $ do
            let checkInline :: Inline Text -> Gen Expectation
                checkInline inline = do
                    let doc  = Doc . pure . Paragraph @Text $ pure inline
                        test = InlineTest { asDoc    = doc
                                          , reparsed = parse [Normalize] (render doc)
                                          , original = inline
                                          , rendered = render doc
                                          }
                    unless (asDoc test == reparsed test) $ do
                        traceM . BL8.unpack . encodePretty $ asDoc test
                        traceM . BL8.unpack . encodePretty $ reparsed test
                    pure $ test `shouldSatisfy` (\x -> asDoc x == reparsed x)

                simpleInline :: Gen (Inline Text)
                simpleInline = Str <$> arbitrary @Text

                simpleInlines :: Gen (Inlines Text)
                simpleInlines = fmap pure simpleInline

            prop "Str" $ checkInline =<< Str <$> arbitrary @Text
            prop "Code" $ checkInline =<< Code <$> arbitrary @Text
            prop "Emph" $ checkInline =<< Emph . pure <$> simpleInline
            prop "Strong" $ checkInline =<< Strong . pure <$> simpleInline
            prop "Link"
                $   checkInline
                =<< Link
                <$> simpleInlines
                <*> arbitrary
                <*> pure Nothing -- arbitrary
            prop "Image"
                $   checkInline
                =<< Image
                <$> simpleInlines
                <*> arbitrary
                <*> pure Nothing --   arbitrary
            xit "SoftBreak" $ shouldBe False True
            -- prop "HardBreak" $ checkInline HardBreak -- Not sure this can be done at start of line
            xit "Task" $ shouldBe False True
--        describe "Inline unlimited" $ do
--            prop "Emph" $ checkInline =<< Emph . Seq.fromList <$> resize
--                1
--                (listOf1 $ arbitrary @(Inline Text))
        describe "Block" $ do
            xit "ThematicBreak" $ shouldBe False True
            xit "Heading" $ shouldBe False True
            xit "CodeBlock" $ shouldBe False True
            xit "Paragraph " $ shouldBe False True
            xit "Question " $ shouldBe False True
            xit "Quote " $ shouldBe False True
            xit "List " $ shouldBe False True
       -- prop "Full document works" $ do
       --     doc <- arbitrary
       --     pure $ doc `shouldBe` parse [] (render doc)
