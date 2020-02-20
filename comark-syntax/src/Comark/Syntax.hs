-- | A definition of Commonmark's AST

module Comark.Syntax
    ( Doc(..)
    -- * Block Elements
    , Blocks
    , Block(..)
    , HeadingLevel(..)
    , ListType(..)
    , Delimiter(..)
    , BulletMarker(..)
    -- * Inline Elements
    , Inlines
    , Inline(..)
    , Language(..)
    , normalize
    , asText
    )
where

import           Control.DeepSeq                ( NFData )
import           Data.Data                      ( Data
                                                , Typeable
                                                )
import           Data.Sequence                  ( Seq
                                                , ViewL(..)
                                                , viewl
                                                , (<|)
                                                )
import           Data.String                    ( IsString(..) )
import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text )
-- | A Document
newtype Doc t = Doc (Blocks t)
  deriving ( Show, Read, Eq
           , Typeable, Data, Generic
           , Functor, Foldable, Traversable
           )

instance NFData t => NFData (Doc t)

instance Semigroup (Doc t) where
    (Doc bs1) <> (Doc bs2) = Doc (bs1 <> bs2)

instance Monoid (Doc t) where
    mempty = Doc mempty

type Blocks t = Seq (Block t)

-- | Block elements
data Block t
  -- ^ Thematic break
  = ThematicBreak
  -- ^ Heading: level, sequnce of inlines that define content
  | Heading HeadingLevel (Inlines t)
  -- ^ Block of code: info string, literal content
  | CodeBlock (Maybe Language) t
  -- ^ Paragraph (a grouped sequence of inlines)
  | Paragraph (Inlines t)
  -- ^ Block Quote (a quoted sequence of blocks)
  | Question (Blocks t) (Maybe (Blocks t))
  | Quote (Blocks t)
  -- ^ List: Type of the list, tightness, a sequnce of blocks (list item)
  | List ListType Bool (Seq (Blocks t))
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic, Functor, Foldable, Traversable
           , NFData)


data Language
    = Unknown Text
    | Haskell
    deriving (Show, Read, Eq, Ord, Typeable, Data, Generic, NFData)

data HeadingLevel
  = Heading1
  | Heading2
  | Heading3
  | Heading4
  | Heading5
  | Heading6
  deriving
    (Show, Read, Eq, Ord, Typeable, Data, Generic, NFData)

data ListType
  = Ordered Delimiter Int
  | Bullet BulletMarker
  deriving
    (Show, Read, Eq, Ord, Typeable, Data, Generic, NFData)


data Delimiter
  = Period
  | Paren
  deriving
    (Show, Read, Eq, Ord, Typeable, Data, Generic, NFData)

data BulletMarker
  = Minus    -- ^ @-@
  | Plus     -- ^ @+@
  | Asterisk -- ^ @*@
  deriving
    (Show, Read, Eq, Ord, Typeable, Data, Generic, NFData)

type Inlines t = Seq (Inline t)

-- | Inline elements
data Inline t
  -- ^ Text (string)
  = Str t
  -- ^ Inline code
  | Code t
  -- ^ Emphasized text (a sequence of inlines)
  | Emph (Inlines t)
  -- ^ Strongly emphasized text (a sequence of inlines)
  | Strong (Inlines t)
  -- ^ Hyperlink: visible link text (sequence of inlines), destination, title
  | Link (Inlines t) t (Maybe t) -- TODO: special types
  -- ^ Image hyperlink: image description, destination, title
  | Image (Inlines t) t (Maybe t) -- TODO: special types
  -- ^ Inline Raw HTML tag
  -- ^ A regular linebreak. A conforming renderer may render a soft
  --   line break in HTML either as line break or as a space.
  | SoftBreak
  -- ^ A line break that is marked as hard (either with spaces or
  --   backslash, see the spec for details). In html it would be rendered
  --   as @<br />@
  | HardBreak
  deriving
    (Show, Read, Eq, Ord, Typeable, Data, Generic, Functor, Foldable, Traversable, NFData)

instance IsString t => IsString (Inline t) where
    fromString = Str . fromString


-- | Consolidate adjacent text nodes
normalize :: Monoid t => Inlines t -> Inlines t
normalize inlines = case viewl inlines of
    Str t :< (viewl -> Str ts :< is) -> normalize (Str (t <> ts) <| is)
    Image i u t :< is -> Image (normalize i) u t <| normalize is
    Link i u t :< is -> Link (normalize i) u t <| normalize is
    Emph i :< is -> Emph (normalize i) <| normalize is
    Strong i :< is -> Strong (normalize i) <| normalize is
    i :< is -> i <| normalize is
    EmptyL -> mempty


-- | Extract textual content from an inline.
--   Note that it extracts only the 'primary' content (the one that is shown in
--   first place). For example it wouldn't extract an URL from the link.
asText :: (Monoid a, IsString a) => Inline a -> a
asText (Str    t    ) = t
asText (Emph   is   ) = foldMap asText is
asText (Strong is   ) = foldMap asText is
asText (Code   t    ) = t
asText (Link  is _ _) = foldMap asText is
asText (Image is _ _) = foldMap asText is
asText SoftBreak      = " "
asText HardBreak      = "\n"
