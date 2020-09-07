{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           AbMarkdown.Parser
import           AbMarkdown.Render
import           AbMarkdown.Syntax
import           AbMarkdown.Elm                 ( )
import           Test.QuickCheck         hiding ( Ordered )
import           Data.Text                      ( Text )
import qualified Data.Text                                    as T
import qualified Data.Sequence                                as Seq
-- import           Data.Aeson

instance Arbitrary Text where
    arbitrary = T.intercalate " " <$> listOf1 bogusWord
      where
        bogusWord :: Gen Text
        bogusWord = fmap T.pack . listOf1 $ elements ['a' .. 'z']

data InlineTest = InlineTest
    { asDoc    :: Doc ()
    , reparsed :: Doc ()
    , original :: Inlines ()
    , rendered :: Text
    }
    deriving (Show, Eq)

data BlockTest = BlockTest
    { reparsedBlocks :: Doc ()
    , originalBlocks :: Doc ()
    , renderedBlocks :: Text
    }
    deriving (Show, Eq)

main :: IO ()
main = hspec $ do
    describe "Rerendering" $ do
        it "Headline" $ do
            "# Headline!" `shouldRerenderAs` "# Headline!"
            "## 2nd level! \n\n" `shouldRerenderAs` "## 2nd level!"
        describe "ThematicBreak" $ do
            it "Simple case is rerendered the same way" $ do
                let
                    b
                        = "Some text\n\
                          \\n\
                          \---\n\
                          \\n\
                          \more text"
                b `shouldRerenderAs` b
            it "Does not fuck up when there are fewer line breaks" $ do
                let
                    b
                        = "Some text\n\
                          \---\n\
                          \more text"
                pendingWith
                    "WTF is up with this? parses as a 2nd level headline!\n\
                            \Turns out this is part of the spec: \n\
                            \https://spec.commonmark.org/0.24/#setext-headings\n\
                            \Pretty bad IMO."
                b `shouldRerenderAs` b
        describe "CodeBlock" $ do
            it "Simple case is rerendered the same way" $ do
                let
                    b
                        = "```haskell\n\
                        \main :: IO ()\n\
                        \main = putStrLn \"Hello world\"\n\
                        \```"
                b `shouldRerenderAs` b
        describe "Multiple headlines with text" $ do
            it "Simple case is rerendered the same way" $ do
                let
                    b
                        = "# Great starwars conversations\n\
                        \\n\
                        \something something something dark side,\n\
                        \something something something complete\n\
                        \\n\
                        \# Other things\n\
                        \\n\
                        \Cars"
                b `shouldRerenderAs` b


    modifyMaxSuccess (const 20)
        . describe "Partial isomorphism `parse == parser . render . parse`"
        $ do
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
                  prop "Task" $ checkInline =<< Task () <$> arbitrary <*> simpleInlines
              xdescribe "Inline unlimited" $ do
                  -- I was naive to think I can get this to work. There are too many illegal
                  -- states that can be expressed in the internal structure. E.g.
                  -- `[Strong [Strong [Str ""]]]` and `[Hardbreak]`
                  prop "Arbitrary inlines"
                      $   checkInlines
                      =<< Seq.fromList
                      <$> listOf1 arbitrary

              describe "Block" $ do
                  prop "ThematicBreak" $ do
                      checkBlocks $ Seq.fromList [ThematicBreak]
                  prop "Heading"
                      $   checkBlocks
                      .   pure
                      =<< Heading
                      <$> arbitrary
                      <*> simpleInlines
                  prop "CodeBlock"
                      $   checkBlocks
                      .   pure
                      =<< CodeBlock
                      <$> arbitrary
                      <*> (fmap (<> "\n") arbitrary)
                      -- FIXME: It fails to parse if ``` is not on it's own line. For some
                      -- reason it is parsed as a codeblcok but also contains the closing ```
                      -- in the text part of the constructor. Fix the parser!
                  prop "Paragraph " $ checkBlocks . pure =<< Paragraph <$> simpleInlines
                  prop "Question"
                      $   checkBlocks
                      .   pure
                      =<< Question ()
                      <$> fmap (pure . Paragraph) simpleInlines
                      <*> oneof
                              [pure Nothing, fmap (Just . pure . Paragraph) simpleInlines]
                  prop "Quote "
                      $   checkBlocks
                      .   pure
                      =<< Quote
                      <$> fmap (pure . Paragraph) simpleInlines
                  prop "List"
                      $   checkBlocks
                      .   pure
                      =<< List
                      <$> arbitrary
                      <*> pure True -- arbitrary --
                      <*> fmap Seq.fromList
                               (listOf1 $ fmap (pure . Paragraph) simpleInlines)

shouldRerenderAs :: Text -> Text -> Expectation
shouldRerenderAs before' after' = do
    let rerendered = render $ parse [Normalize] before'
    rerendered `shouldBe` after'

checkBlocks :: Blocks () -> Gen Expectation
checkBlocks bs = do
    let doc  = Doc bs
        test = BlockTest { reparsedBlocks = parse [Normalize] $ render doc
                         , originalBlocks = doc
                         , renderedBlocks = render doc
                         }
    pure $ test `shouldSatisfy` (\x -> originalBlocks x == reparsedBlocks x)
checkInline :: Inline () -> Gen Expectation
checkInline = checkInlines . pure

checkInlines :: Inlines () -> Gen Expectation
checkInlines inlines = do
    let doc  = Doc . pure . Paragraph $ normalize inlines
        test = InlineTest { asDoc    = doc
                          , reparsed = parse [Normalize] (render doc)
                          , original = inlines
                          , rendered = render doc
                          }
    pure $ test `shouldSatisfy` (\x -> asDoc x == reparsed x)

simpleInline :: Gen (Inline ())
simpleInline = Str <$> arbitrary @Text

simpleInlines :: Gen (Inlines ())
simpleInlines = fmap pure simpleInline
