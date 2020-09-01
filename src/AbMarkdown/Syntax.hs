-- | A definition of Commonmark's AST

module AbMarkdown.Syntax
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
    , TaskStatus(..)
    , Language(..)
    , normalize
    , LinkRef(..)
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
                                                , fromList
                                                , (<|)
                                                )
import           Data.String                    ( IsString(..) )
import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text )
import           Test.QuickCheck
import qualified Data.List                                    as L
import           Test.QuickCheck.Arbitrary.ADT

-- | A Document
newtype Doc t = Doc (Blocks t)
  deriving ( Show, Read, Eq, Typeable, Data, Generic, Functor, Foldable, Traversable)
instance (Eq t, Arbitrary t) => Arbitrary (Doc t) where
    arbitrary = genericArbitrary

instance NFData t => NFData (Doc t)

instance Semigroup (Doc t) where
    (Doc bs1) <> (Doc bs2) = Doc (bs1 <> bs2)

instance Monoid (Doc t) where
    mempty = Doc mempty

type Blocks t = Seq (Block t)

-- | Block elements
data Block t -- ^ Thematic break
  = ThematicBreak
  | Heading HeadingLevel (Inlines t)
  | CodeBlock (Maybe Language) t
  | Paragraph (Inlines t)
  | Question (Blocks t) (Maybe (Blocks t))
  | Quote (Blocks t) -- ^ Block Quote (a quoted sequence of blocks)
  | List ListType Bool (Seq (Blocks t)) -- ^ List: Type of the list, tightness, a sequnce of blocks (list item)
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic, Functor, Foldable, Traversable
           , NFData)

instance (Eq t, Arbitrary t) => Arbitrary (Block t) where
    arbitrary = oneof
        [ pure ThematicBreak
        , Heading <$> arbitrary <*> scaleDown arbitrary
        , CodeBlock <$> arbitrary <*> arbitrary
        , Paragraph <$> scaleDown arbitrary
        , Question <$> scaleDown arbitrary <*> scaleDown arbitrary
        , Quote <$> scaleDown arbitrary
        , List <$> arbitrary <*> arbitrary <*> scaleDown arbitrary
        ]
      where
        scaleDown :: Gen a -> Gen a
        scaleDown = scale (`div` 2)


data TaskStatus
    = Todo
    | Done
    deriving (Show, Eq, Ord, Read, Typeable, NFData, Data, Generic)

instance Arbitrary TaskStatus where
    arbitrary = genericArbitrary

data Language
    = Unknown Text
    | Haskell
    deriving (Show, Read, Eq, Ord, Typeable, Data, Generic, NFData)
instance Arbitrary Language where
    arbitrary = elements [Haskell, Unknown "elm", Unknown "julia"]

data HeadingLevel
  = Heading1
  | Heading2
  | Heading3
  | Heading4
  | Heading5
  | Heading6
  deriving
    (Show, Read, Eq, Ord, Typeable, Data, Generic, NFData)
instance Arbitrary HeadingLevel where
    arbitrary = genericArbitrary

data ListType
  = Ordered Delimiter Int
  | Bullet BulletMarker
  deriving
    (Show, Read, Eq, Ord, Typeable, Data, Generic, NFData)
instance Arbitrary ListType where
    arbitrary = genericArbitrary


data Delimiter
  = Period
  | Paren
  deriving
    (Show, Read, Eq, Ord, Typeable, Data, Generic, NFData)
instance Arbitrary Delimiter where
    arbitrary = genericArbitrary

data BulletMarker
  = Minus    -- ^ @-@
  | Plus     -- ^ @+@
  | Asterisk -- ^ @*@
  deriving
    (Show, Read, Eq, Ord, Typeable, Data, Generic, NFData)
instance Arbitrary BulletMarker where
    arbitrary = genericArbitrary

type Inlines t = Seq (Inline t)

newtype LinkRef = LinkRef
    {unLinkRef :: Text
    } deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)
    deriving newtype (IsString, NFData)

instance Arbitrary LinkRef where
    arbitrary =
        elements ["https://google.com", "dn.se", "mailto:something@something.com"]

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
  | Link (Inlines t) LinkRef (Maybe t)
  -- ^ FIXME: Not sure how the `Maybe t` is supposed to work. Seems it should be part of
  -- LinkRef.
  | Image (Inlines t) LinkRef (Maybe t) -- TODO: special types
  -- ^ Image hyperlink: image description, destination, title
  | SoftBreak
  -- ^ A regular linebreak. A conforming renderer may render a soft
  --   line break in HTML either as line break or as a space.
  | HardBreak
  -- ^ A line break that is marked as hard (either with spaces or
  --   backslash, see the spec for details). In html it would be rendered
  --   as @<br />@
  | Task TaskStatus (Inlines t) -- TODO: Add `Maybe Deadline`
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic, Functor, Foldable, Traversable, NFData)

instance {-# Overlapping #-} (Eq t, Arbitrary t) => Arbitrary (Inlines t) where
    arbitrary = do
        s <- L.dropWhileEnd isBreak . L.dropWhile isBreak <$> listOf1 arbitrary
        if null s then arbitrary else pure $ fromList s

isBreak :: Eq t => Inline t -> Bool
isBreak = flip elem [SoftBreak, HardBreak]

arbitraryInlineNoBreak :: (Eq t, Arbitrary t) => Gen (Inline t)
arbitraryInlineNoBreak = do
    s <- arbitrary
    if isBreak s then arbitraryInlineNoBreak else pure s

instance (Eq t, Arbitrary t) => Arbitrary (Inline t) where
    arbitrary = oneof
        [ Str <$> arbitrary
        , Code <$> arbitrary
        , Emph . fromList <$> scaleDown (listOf1 arbitraryInlineNoBreak)
        , Strong . fromList <$> scaleDown (listOf1 arbitraryInlineNoBreak)
        , Link <$> scaleDown arbitrary <*> arbitrary <*> arbitrary
        , Image <$> scaleDown arbitrary <*> arbitrary <*> arbitrary
        , pure SoftBreak
        , pure HardBreak
        , Task <$> arbitrary <*> scaleDown arbitrary
        ]

      where
        scaleDown :: Gen a -> Gen a
        scaleDown = scale (`div` 3)

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
    Task s i :< is -> Task s (normalize i) <| normalize is
    Str t :< is -> Str t <| normalize is
    Code t :< is -> Code t <| normalize is
    HardBreak :< is -> HardBreak <| normalize is
    SoftBreak :< is -> SoftBreak <| normalize is
    EmptyL -> mempty


-- | Extract textual content from an inline.
--   Note that it extracts only the 'primary' content (the one that is shown in
--   first place). For example it wouldn't extract an URL from the link.
asText :: (Monoid a, IsString a) => Inline a -> a
asText = \case
    Str    t       -> t
    Emph   is      -> foldMap asText is
    Strong is      -> foldMap asText is
    Code   t       -> t
    Link  is _ _   -> foldMap asText is
    Image is _ _   -> foldMap asText is
    SoftBreak      -> " "
    HardBreak      -> "\n"
    Task status is -> renderStatus status <> " " <> foldMap asText is
  where
    renderStatus :: IsString a => TaskStatus -> a
    renderStatus = \case
        Todo -> fromString "[ ]"
        Done -> fromString "[x]"
