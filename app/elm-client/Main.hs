{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Elm.Derive              hiding ( defaultOptions )
import           Elm.Module
import           Elm.Versions
import           Elm.TyRep
import           Data.Proxy
import           AbMarkdown.Syntax
import           Data.Aeson
import           Options.Generic
import           Data.UUID
import           Control.Monad
import           System.FilePath
import           Test.QuickCheck
import qualified Data.List                                    as L


deriveElmDef defaultOptions ''Doc
deriveElmDef defaultOptions ''Blocks
deriveElmDef defaultOptions ''Block
deriveElmDef defaultOptions ''HeadingLevel
deriveElmDef defaultOptions ''Language
deriveElmDef defaultOptions ''ListType
deriveElmDef defaultOptions ''Delimiter
deriveElmDef defaultOptions ''BulletMarker
deriveElmDef defaultOptions ''Inline
deriveElmDef defaultOptions ''TaskStatus
deriveElmDef defaultOptions {unwrapUnaryRecords = True} ''LinkRef
deriveElmDef defaultOptions ''Inlines


data Opts = Opts
    { output       :: FilePath <?> "Folder to write to"
    , includeTests :: Bool <?> "Also write a test suite"
    }
    deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Opts (Helpful outputFolder) (Helpful writeTests) <- getRecord "Elm client generator"
    writeFile (outputFolder </> "Markdown.elm") elmClient
    when writeTests $ do
        elmTests <- mkElmTests
        writeFile (outputFolder </> "MarkdownTests.elm") elmTests


mkElmTests :: IO String
mkElmTests = ((imports <> describe) <>) <$> tests
  where
    imports
        = "module MarkdownTests exposing (..)\n\
        \import Expect exposing (Expectation)\n\
        \import Fuzz exposing (Fuzzer, int, list, string)\n\
        \import Json.Decode as D\n\
        \import Markdown exposing (..)\n\
        \import Test exposing (..)\n"

    describe
        = "suite : Test\n\
          \suite =\
          \    describe \"AbMarkdown Elm client\""

    tests :: IO String
    tests = do
        linkRefSamples <- replicateM 50 $ generate (resize 1 $ arbitrary @LinkRef)
        docSamples     <- fmap mconcat $ do
            sequence -- start with some smaller tests
                [ replicateM 100 $ generate (resize 0 $ arbitrary @(Doc UUID))
                , replicateM 100 $ generate (resize 1 $ arbitrary @(Doc UUID))
                , replicateM 100 $ generate (resize 3 $ arbitrary @(Doc UUID))
                , replicateM 100 $ generate (resize 6 $ arbitrary @(Doc UUID))
                , replicateM 100 $ generate (resize 10 $ arbitrary @(Doc UUID))
                , replicateM 100 $ generate (resize 20 $ arbitrary @(Doc UUID))
                ]
        pure
            $  "\n        ["
            <> L.intercalate
                   "\n        ,"
                   (  zipWith (mkTest "jsonDecDoc (D.string)") [1 ..] docSamples
                   <> zipWith (mkTest "jsonDecLinkRef")        [1 ..] linkRefSamples
                   )
            <> "\n        ]"


    mkTest :: ToJSON a => String -> Int -> a -> String
    mkTest decoder i a =
        "test \""
            <> decoder
            <> ": parse test nr "
            <> show i
            <> "\" <| \
        \ \\_ -> Expect.equal (Result.map (always ()) <| \
        \ D.decodeString ("
            <> decoder
            <> ") "
            <> show (encode a)
            <> ") (Ok ())"

elmClient :: String
elmClient = elmImports <> elmDefs
  where
    elmImports = unlines
        [ moduleHeader Elm0p19 "Markdown"
        , ""
        , "import Json.Decode"
        , "import Json.Encode exposing (Value)"
        , "-- The following module comes from bartavelle/json-helpers"
        , "import Json.Helpers exposing (..)"
        , "import Dict exposing (Dict)"
        , "import Set exposing (Set)"
        , ""
        , ""
        ]
    elmDefs = makeModuleContentWithAlterations
        alterations
        [ DefineElm (Proxy @(Block Text))
        , DefineElm (Proxy @(Blocks Text))
        , DefineElm (Proxy @(Doc Text))
        , DefineElm (Proxy @HeadingLevel)
        , DefineElm (Proxy @Language)
        , DefineElm (Proxy @ListType)
        , DefineElm (Proxy @Delimiter)
        , DefineElm (Proxy @LinkRef)
        , DefineElm (Proxy @BulletMarker)
        , DefineElm (Proxy @(Inline Text))
        , DefineElm (Proxy @(Inlines Text))
        , DefineElm (Proxy @TaskStatus)
        ]

    alterations :: ETypeDef -> ETypeDef
    alterations = recAlterType typeAlterations

    typeAlterations :: EType -> EType
    typeAlterations = \case
        ETyCon (ETCon "Seq") -> ETyCon (ETCon "List")
        td                   -> defaultTypeAlterations td
