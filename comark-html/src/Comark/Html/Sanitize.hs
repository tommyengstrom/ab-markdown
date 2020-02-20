{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Comark.Html.Sanitize where

import qualified Data.Generics                                as Generics
import           Data.Sequence                                as Seq
import           Data.Text                      ( Text )
import qualified Data.Text                                    as Text

import           Comark.Syntax

removeHtml :: Doc Text -> Doc Text
removeHtml = removeUnsafeLinks


removeUnsafeLinks :: Doc Text -> Doc Text
removeUnsafeLinks = Generics.everywhere (Generics.mkT withInline)
  where
    withInline = \case
        Link is dest title | isUnsafeDestination dest -> Link is "" title
        Image is dest title | isUnsafeDestination dest -> Image is "" title
        b -> b

isUnsafeDestination :: Text -> Bool
isUnsafeDestination t = safeDataProtocol || not unsafeProtocol
  where
    unsafeProtocol = and $ map (`Text.isPrefixOf` Text.toLower t)
                               ["javascript:", "vbscript:", "file:", "data:"]
    safeDataProtocol = and $ map
        (`Text.isPrefixOf` Text.toLower t)
        ["data:image/png", "data:image/jpeg", "data:image/webp", "data:image/gif"]

