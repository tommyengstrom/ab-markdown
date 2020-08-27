module Main where

import           Test.Hspec
--import           Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Partial isomorphism `parse == parser . render . parse`" $ do
        it "Link blocks work " $ shouldBe False True
        it "Image blocks work " $ shouldBe False True
        it "List blocks work " $ shouldBe False True
        it "Full document works " $ shouldBe False True
