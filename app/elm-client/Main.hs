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
import           Control.Monad
import           System.FilePath
import           Test.QuickCheck
import qualified Data.List                                    as L
import           Data.UUID
--import           Data.Traversable


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
deriveElmDef defaultOptions ''LinkRef
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
        writeFile (outputFolder </> "Tests.elm") elmTests


mkElmTests :: IO String
mkElmTests = (describe <>) <$> tests
  where
    describe
        = "suite : Test\
          \suite =\
          \    describe \"AbMarkdown Elm client\""

    tests = do
        samples <- fmap (fmap withUUID . mconcat) $ do
            sequence -- start with some smaller tests
                     [replicateM 10 $ generate (resize 1 $ arbitrary @(Doc ()))
               -- , replicateM 30 $ generate (resize 3 $ arbitrary @(Doc ()))
               -- , replicateM 60 $ generate (resize 6 $ arbitrary @(Doc ()))
                                                                               ]

        pure
            $  "\n        ["
            <> L.intercalate "\n        ," (zipWith mkTest [1 ..] samples)
            <> "\n        ]"

    mkTest :: Int -> Doc UUID -> String
    mkTest i d =
        "test \"Parse test nr "
            <> show i
            <> "\" <| \
        \ \\_ -> Expect.equal (Result.map (always ()) <| \
        \ D.decodeString decodeDoc "
            <> show (encode d)
            <> ") (Ok ())"


elmClient :: String
elmClient = elmImports <> elmDefs
  where
    elmImports =
        unlines
                [ moduleHeader Elm0p18 "Markdown"
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
            ++ elmDefs
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
