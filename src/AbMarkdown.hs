module AbMarkdown
    ( module X
    , module AbMarkdown
    , render
    )
where

import           AbMarkdown.Syntax
import qualified AbMarkdown.Syntax                            as X
                                                ( Doc(..)
                                                , Blocks
                                                , Block(..)
                                                , Inlines
                                                , Inline(..)
                                                , updateBlock
                                                , getBlock
                                                , updateInline
                                                , getInline
                                                , TaskStatus(..)
                                                , HeadingLevel(..)
                                                , ListType(..)
                                                , Language(..)
                                                , Delimiter(..)
                                                , BulletMarker(..)
                                                , LinkRef(..)
                                                )
import           AbMarkdown.Render              ( render )
import qualified AbMarkdown.Parser                            as P
import           Data.UUID
import           Data.Text                      ( Text )

parse :: Text -> Doc UUID
parse = withUUID . P.parse [P.Normalize]
