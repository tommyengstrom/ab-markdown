module AbMarkdown.Parser.Options
    ( ParserOption(..)
    , ParserOptions()
    , parserOptions
    , _poNormalize
    , _poLinkReferences
    , _poParseEmphasis
    )
where

import           Control.Applicative            ( (<|>) )
import           Data.Monoid                    ( Endo(Endo, appEndo) )

import           AbMarkdown.Parser.Reference

data ParserOption
  = -- | Consolidate adjacent text nodes.
    Normalize
    -- | Predefine
    --   <http://spec.commonmark.org/0.20/#link-reference-definition link reference defenitions>.
    --
    --   References are represented with a mapping from a
    --   <http://spec.commonmark.org/0.20/#link-text link text> to a pair of a
    --   <http://spec.commonmark.org/0.20/#link-destination link destination>
    --   and an optional
    --   <http://spec.commonmark.org/0.20/#link-title link title>.
    --
    --   During parsing the link references defined in a document would be
    --   collected into additional mapping. When link references are being
    --   mapping defined in options takes precedence over mapping found in
    --   the document.
    --
    --   TODO: Examples
  | LinkReferences (LinkText -> Maybe (LinkDestination, Maybe LinkTitle))

data ParserOptions = ParserOptions
    { _poNormalize      :: Bool
    , _poLinkReferences :: LinkText -> Maybe (LinkDestination, Maybe LinkTitle)
    , _poParseEmphasis  :: Bool
    }
instance Semigroup ParserOptions where
    a <> b =
        b { _poLinkReferences = \t -> _poLinkReferences b t <|> _poLinkReferences a t }

instance Monoid ParserOptions where
    mempty = ParserOptions { _poNormalize      = False
                           , _poLinkReferences = const Nothing
                           , _poParseEmphasis  = True
                           }

parserOptions :: [ParserOption] -> ParserOptions
parserOptions = ($ mempty) . appEndo . foldMap (Endo . optFn)
  where
    optFn :: ParserOption -> ParserOptions -> ParserOptions
    optFn Normalize o = o { _poNormalize = True }
    optFn (LinkReferences f) o =
        o { _poLinkReferences = \t -> f t <|> _poLinkReferences o t }
