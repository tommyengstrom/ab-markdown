{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AbMarkdown.Elm where

import           Elm.Derive              hiding ( defaultOptions )
import           Elm.Module
import           Elm.Versions
import           Elm.TyRep
import           Data.Proxy
import           Data.Text                      ( Text )
import           AbMarkdown.Syntax
import           Data.Aeson

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


main :: IO ()
main = do
    let elmModule =
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
            , DefineElm (Proxy @BulletMarker)
            , DefineElm (Proxy @(Inline Text))
            , DefineElm (Proxy @(Inlines Text))
            , DefineElm (Proxy @TaskStatus)
            ]
        fName = "/home/tommy/git/awebitious/apps/webapp/src/Markdown.elm"
    writeFile fName $ elmModule
    putStrLn $ "Wrote file: " <> fName


alterations :: ETypeDef -> ETypeDef
alterations = recAlterType typeAlterations

typeAlterations :: EType -> EType
typeAlterations = \case
    ETyCon (ETCon "Seq") -> ETyCon (ETCon "List")
    td                   -> defaultTypeAlterations td
