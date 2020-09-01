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
import qualified Data.Sequence                                as Seq
-- import           Data.Aeson

instance Arbitrary Text where
    arbitrary = T.intercalate " " <$> listOf1 bogusWord
      where
        bogusWord :: Gen Text
        bogusWord = fmap T.pack . listOf1 $ elements ['a' .. 'z']

data InlineTest = InlineTest
    { asDoc    :: Doc Text
    , reparsed :: Doc Text
    , original :: Inlines Text
    , rendered :: Text
    }
    deriving (Show, Eq)

main :: IO ()
main = hspec $ do
    describe "AbMarkdown.Parser" $ do
        xit "Can parser question without answer" $ shouldBe False True
        xit "Can parser question with answer" $ shouldBe False True
    describe "Partial isomorphism `parse == parser . render . parse`" $ do
        let checkInline :: Inline Text -> Gen Expectation
            checkInline = checkInlines . pure

            checkInlines :: Inlines Text -> Gen Expectation
            checkInlines inlines = do
                let doc  = Doc . pure . Paragraph @Text $ normalize inlines
                    test = InlineTest { asDoc    = doc
                                      , reparsed = parse [Normalize] (render doc)
                                      , original = inlines
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
        describe "Inline without recursion" $ do
            prop "Str" $ checkInline =<< Str <$> arbitrary @Text
            prop "Code" $ checkInline =<< Code <$> arbitrary @Text
            prop "Emph" $ checkInline =<< Emph . pure <$> simpleInline
            prop "Strong" $ checkInline =<< Strong . pure <$> simpleInline
            prop "Link"
                $   checkInline
                =<< Link
                <$> simpleInlines
                <*> arbitrary
                <*> pure Nothing -- arbitrary -- FIXME: What is this?
            prop "Image"
                $   checkInline
                =<< Image
                <$> simpleInlines
                <*> arbitrary
                <*> pure Nothing --   arbitrary -- FIXME: What is this?
            prop "SoftBreak" $ do
                -- Breaks can only be between other stuff
                before' <- simpleInline
                after'  <- simpleInline
                checkInlines $ Seq.fromList [before', SoftBreak, after']
            prop "HardBreak" $ do
                -- Breaks can only be between other stuff
                before' <- simpleInline
                after'  <- simpleInline
                checkInlines $ Seq.fromList [before', HardBreak, after']
            prop "Task" $ checkInline =<< Task <$> arbitrary <*> simpleInlines
        xdescribe "Inline unlimited" $ do
            -- I was naive to think I can get this to work. There are too many illegal
            -- states that can be expressed in the internal structure. E.g.
            -- `[Strong [Strong [Str ""]]]` and `[Hardbreak]`
            prop "Arbitrary inlines" $ checkInlines =<< Seq.fromList <$> listOf1 arbitrary
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
