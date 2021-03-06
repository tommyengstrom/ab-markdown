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
    , withUUID
    , updateBlock
    , updateInline
    , getBlock
    , getInline
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
import           Control.Applicative
import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text )
import qualified Data.Text                                    as T
import           Data.Maybe                     ( catMaybes )
import           Test.QuickCheck         hiding ( Ordered )
import qualified Data.List                                    as L
import           Test.QuickCheck.Arbitrary.ADT
import           Data.Aeson
import qualified Data.HashMap.Strict                          as HM
import           Data.UUID                      ( UUID )
import           System.Random
import           Data.Foldable
import           Control.Monad.State


-- | A Document
newtype Doc id = Doc (Blocks id)
  deriving stock ( Show, Read, Eq, Typeable, Data, Generic, Functor, Foldable, Traversable)
  deriving anyclass (ToJSON, FromJSON)
instance Arbitrary (Doc ()) where
    arbitrary = genericArbitrary

instance Arbitrary (Doc UUID) where
    arbitrary = withUUID <$> arbitrary

instance NFData id => NFData (Doc id)

withUUID :: Doc () -> Doc UUID
withUUID doc = fst $ runState (traverse addUUID doc) (mkStdGen seed)
  where
    addUUID :: () -> State StdGen UUID
    addUUID () = do
        gen <- get
        let (uuid, gen') = random gen
        put gen'
        pure uuid


    seed :: Int
    seed = case doc of
        Doc bs -> sum $ fmap blockseed bs

    blockseed :: Block () -> Int
    blockseed = \case
        ThematicBreak   -> 1
        Heading   _ is  -> sum $ fmap inlineseed is
        CodeBlock _ t   -> T.length t
        Paragraph is    -> sum $ fmap inlineseed is
        Question () _ _ -> 1 -- ^ Don't count things with id to allow concurrent updates
        Quote bs        -> sum $ fmap blockseed bs
        List _ _ bs     -> sum $ fmap (sum . fmap blockseed) bs

    inlineseed :: Inline () -> Int
    inlineseed = \case
        Str    t       -> T.length t
        Code   t       -> T.length t
        Emph   is      -> sum $ fmap inlineseed is
        Strong is      -> sum $ fmap inlineseed is
        Link  is ref _ -> sum (fmap inlineseed is) + linkRefseed ref
        Image is ref _ -> sum (fmap inlineseed is) + linkRefseed ref
        SoftBreak      -> 1
        HardBreak      -> 1
        Task () _ _    -> 1 -- ^ Don't count things with id to allow concurrent updates
    linkRefseed :: LinkRef -> Int
    linkRefseed = T.length . unLinkRef

getBlock :: UUID -> Doc UUID -> Maybe (Block UUID)
getBlock key (Doc blocks) = go blocks
  where
    go :: Blocks UUID -> Maybe (Block UUID)
    go bs = case viewl bs of
        EmptyL                -> Nothing
        ThematicBreak :< rest -> go rest
        Heading   _ _ :< rest -> go rest
        CodeBlock _ _ :< rest -> go rest
        Paragraph _   :< rest -> go rest
        Quote     bs' :< rest -> go bs' <|> go rest
        List _ _ bs'  :< rest -> headMay (catMaybes . toList $ fmap go bs') <|> go rest
        q@(Question key' bs' bs'') :< rest
            | key' == key -> Just q
            | otherwise   -> go bs' <|> maybe Nothing go bs'' <|> go rest

headMay :: [a] -> Maybe a
headMay = \case
    a : _ -> Just a
    []    -> Nothing

updateBlock :: UUID -> (Block UUID -> Block UUID) -> Doc UUID -> Doc UUID
updateBlock key f (Doc blocks) = Doc $ go blocks
  where
    go :: Blocks UUID -> Blocks UUID
    go bs = case viewl bs of
        EmptyL                            -> mempty
        ThematicBreak             :< rest -> pure ThematicBreak <> go rest
        x@(Heading   _hl   _il  ) :< rest -> pure x <> go rest
        x@(CodeBlock _lang _code) :< rest -> pure x <> go rest
        x@(Paragraph _il        ) :< rest -> pure x <> go rest
        Quote inneBs              :< rest -> pure (Quote $ go inneBs) <> go rest
        List lt x ls              :< rest -> pure (List lt x $ fmap go ls) <> go rest
        q@(Question key' qBs mABs) :< rest
            | key' == key -> pure (f q) <> rest
            | otherwise   -> pure (Question key' (go qBs) (fmap go mABs)) <> rest


getInline :: UUID -> Doc UUID -> Maybe (Inline UUID)
getInline key (Doc blocks) = go blocks
  where
    go :: Blocks UUID -> Maybe (Inline UUID)
    go bs = case viewl bs of
        EmptyL                      -> Nothing
        ThematicBreak       :< rest -> go rest
        Heading   _ il      :< rest -> goIL il <|> go rest
        CodeBlock _ _       :< rest -> go rest
        Paragraph il        :< rest -> goIL il <|> go rest
        Quote     bs'       :< rest -> go bs' <|> go rest
        List _ _ bs' :< rest -> headMay (catMaybes . toList $ fmap go bs') <|> go rest
        Question _ bs' bs'' :< rest -> go bs' <|> maybe Nothing go bs'' <|> go rest

    goIL :: Inlines UUID -> Maybe (Inline UUID)
    goIL inlines = case viewl inlines of
        EmptyL               -> Nothing
        Str    _     :< rest -> goIL rest
        Code   _     :< rest -> goIL rest
        Emph   il    :< rest -> goIL il <|> goIL rest
        Strong il    :< rest -> goIL il <|> goIL rest
        Link  il _ _ :< rest -> goIL il <|> goIL rest
        Image il _ _ :< rest -> goIL il <|> goIL rest
        SoftBreak    :< rest -> goIL rest
        HardBreak    :< rest -> goIL rest
        t@(Task key' _ il) :< rest | key' == key -> Just t
                                   | otherwise   -> goIL il <|> goIL rest



updateInline :: UUID -> (Inline UUID -> Inline UUID) -> Doc UUID -> Doc UUID
updateInline key f (Doc blocks) = Doc $ go blocks
  where
    go :: Blocks UUID -> Blocks UUID
    go bs = case viewl bs of
        EmptyL -> mempty
        ThematicBreak :< rest -> pure ThematicBreak <> go rest
        Heading hl il :< rest -> pure (Heading hl (goIL il)) <> go rest
        x@(CodeBlock _lang _code) :< rest -> pure x <> go rest
        Paragraph il :< rest -> pure (Paragraph (goIL il)) <> go rest
        Quote inneBs :< rest -> pure (Quote $ go inneBs) <> go rest
        List lt x ls :< rest -> pure (List lt x $ fmap go ls) <> go rest
        Question key' qBs mABs :< rest ->
            pure (Question key' (go qBs) (fmap go mABs)) <> rest

    goIL :: Inlines UUID -> Inlines UUID
    goIL inlines = case viewl inlines of
        EmptyL               -> mempty
        x@(Str  _)   :< rest -> pure x <> goIL rest
        x@(Code _)   :< rest -> pure x <> goIL rest
        Emph   il    :< rest -> pure (Emph $ goIL il) <> goIL rest
        Strong il    :< rest -> pure (Strong $ goIL il) <> goIL rest
        Link  il r t :< rest -> pure (Link (goIL il) r t) <> goIL rest
        Image il r t :< rest -> pure (Image (goIL il) r t) <> goIL rest
        SoftBreak    :< rest -> pure SoftBreak <> rest
        HardBreak    :< rest -> pure HardBreak <> rest
        t@(Task key' ts il) :< rest
            | key' == key -> pure (f t) <> rest
            | otherwise   -> pure (Task key' ts (goIL il)) <> goIL rest




instance Semigroup (Doc id) where
    (Doc bs1) <> (Doc bs2) = Doc (bs1 <> bs2)

instance Monoid (Doc id) where
    mempty = Doc mempty

type Blocks id = Seq (Block id)

-- | Block elements
data Block id
  = ThematicBreak
  | Heading HeadingLevel (Inlines id)
  | CodeBlock (Maybe Language) Text
  | Paragraph (Inlines id)
  | Question id (Blocks id) (Maybe (Blocks id))
  | Quote (Blocks id) -- ^ Block Quote (a quoted sequence of blocks)
  | List ListType Bool (Seq (Blocks id)) -- ^ List: Type of the list, tightness, a sequnce of blocks (list item)
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic, Functor, Foldable, Traversable
           , FromJSON, NFData)

instance ToJSON id =>  ToJSON (Block id) where
    toJSON = \case
        ThematicBreak ->
            Object $ HM.fromList [("tag", String "ThematicBreak"), ("contents", Null)]
        a -> genericToJSON defaultOptions a

instance (Eq id, Arbitrary id) => Arbitrary (Block id) where
    arbitrary = oneof
        [ pure ThematicBreak
        , Heading <$> arbitrary <*> scaleDown arbitrary
        , CodeBlock <$> arbitrary <*> arbitraryText
        , Paragraph <$> scaleDown arbitrary
        , Question <$> arbitrary <*> scaleDown arbitrary <*> scaleDown arbitrary
        , Quote <$> scaleDown arbitrary
        , List <$> arbitrary <*> arbitrary <*> scaleDown arbitrary
        ]
      where
        scaleDown :: Gen a -> Gen a
        scaleDown = scale (`div` 2)


data TaskStatus
    = Todo
    | Done
    deriving (Show, Eq, Ord, Read, Typeable, NFData, Data, Generic, ToJSON, FromJSON)

instance Arbitrary TaskStatus where
    arbitrary = genericArbitrary

data Language
    = Unknown Text
    | Haskell
    deriving (Show, Read, Eq, Ord, Typeable, Data, Generic, NFData, ToJSON, FromJSON)
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
    (Show, Read, Eq, Ord, Typeable, Data, Generic, NFData, ToJSON, FromJSON)
instance Arbitrary HeadingLevel where
    arbitrary = genericArbitrary

data ListType
  = Ordered Delimiter Int
  | Bullet BulletMarker
  deriving
    (Show, Read, Eq, Ord, Typeable, Data, Generic, NFData, ToJSON, FromJSON)
instance Arbitrary ListType where
    arbitrary =
        oneof
            [ Bullet <$> arbitrary
            , Ordered <$> arbitrary <*> fmap getNonNegative arbitrary
            ]


data Delimiter
  = Period
  | Paren
  deriving
    (Show, Read, Eq, Ord, Typeable, Data, Generic, NFData, ToJSON, FromJSON)
instance Arbitrary Delimiter where
    arbitrary = genericArbitrary

data BulletMarker
  = Minus    -- ^ @-@
  | Plus     -- ^ @+@
  | Asterisk -- ^ @*@
  deriving
    (Show, Read, Eq, Ord, Typeable, Data, Generic, NFData, ToJSON, FromJSON)
instance Arbitrary BulletMarker where
    arbitrary = genericArbitrary

type Inlines id = Seq (Inline id)

newtype LinkRef = LinkRef
    { unLinkRef :: Text
    }
    deriving stock (Show, Read, Eq, Ord, Typeable, Data, Generic)
    deriving newtype (IsString, NFData, ToJSON, FromJSON)

instance Arbitrary LinkRef where
    arbitrary =
        elements ["https://google.com", "dn.se", "mailto:something@something.com"]

-- | Inline elements
data Inline id
  -- ^ Text (string)
  = Str Text
  -- ^ Inline code
  | Code Text
  -- ^ Emphasized text (a sequence of inlines)
  | Emph (Inlines id)
  -- ^ Strongly emphasized text (a sequence of inlines)
  | Strong (Inlines id)
  -- ^ Hyperlink: visible link text (sequence of inlines), destination, title
  | Link (Inlines id) LinkRef (Maybe Text)
  -- ^ FIXME: Not sure how the `Maybe t` is supposed to work. Seems it should be part of
  -- LinkRef.
  | Image (Inlines id) LinkRef (Maybe Text) -- TODO: special types
  -- ^ Image hyperlink: image description, destination, title
  | SoftBreak
  -- ^ A regular linebreak. A conforming renderer may render a soft
  --   line break in HTML either as line break or as a space.
  | HardBreak
  -- ^ A line break that is marked as hard (either with spaces or
  --   backslash, see the spec for details). In html it would be rendered
  --   as @<br />@
  | Task id TaskStatus (Inlines id) -- TODO: Add `Maybe Deadline`
  deriving ( Show, Read, Eq, Ord, Typeable, Data, Generic, Functor, Foldable, Traversable
           , NFData, FromJSON)

-- | FIXME: This is due to using the broken elm generator. Aeson used to do thing like in
-- 2016...
instance ToJSON id => ToJSON (Inline id) where
    toJSON = \case
        SoftBreak ->
            Object $ HM.fromList [("tag", String "SoftBreak"), ("contents", Null)]
        HardBreak ->
            Object $ HM.fromList [("tag", String "HardBreak"), ("contents", Null)]
        a -> genericToJSON defaultOptions a

instance {-# Overlapping #-} (Arbitrary id, Eq id) => Arbitrary (Inlines id) where
    arbitrary = do
        s <- L.dropWhileEnd isBreak . L.dropWhile isBreak <$> listOf1 arbitrary
        if null s then arbitrary else pure $ fromList s

isBreak :: Eq t => Inline t -> Bool
isBreak = flip elem [SoftBreak, HardBreak]

arbitraryInlineNoBreak :: (Eq t, Arbitrary t) => Gen (Inline t)
arbitraryInlineNoBreak = do
    s <- arbitrary
    if isBreak s then arbitraryInlineNoBreak else pure s

instance (Eq id, Arbitrary id) => Arbitrary (Inline id) where
    arbitrary = oneof
        [ Str <$> arbitraryText
        , Code <$> arbitraryText
        , Emph . fromList <$> scaleDown (listOf1 arbitraryInlineNoBreak)
        , Strong . fromList <$> scaleDown (listOf1 arbitraryInlineNoBreak)
        , Link <$> scaleDown arbitrary <*> arbitrary <*> oneof
            [pure Nothing, Just <$> arbitraryText]
        , Image <$> scaleDown arbitrary <*> arbitrary <*> oneof
            [pure Nothing, Just <$> arbitraryText]
        , pure SoftBreak
        , pure HardBreak
        , Task <$> arbitrary <*> arbitrary <*> scaleDown arbitrary
        ]

      where
        scaleDown :: Gen a -> Gen a
        scaleDown = scale (`div` 3)

arbitraryText :: Gen Text
arbitraryText = T.intercalate " " <$> listOf1 bogusWord
  where
    bogusWord :: Gen Text
    bogusWord = fmap T.pack . listOf1 $ elements ['a' .. 'z']


-- | Consolidate adjacent text nodes
normalize :: Inlines id -> Inlines id
normalize inlines = case viewl inlines of
    Str t :< (viewl -> Str ts :< is) -> normalize (Str (t <> ts) <| is)
    Image i u t :< is -> Image (normalize i) u t <| normalize is
    Link i u t :< is -> Link (normalize i) u t <| normalize is
    Emph i :< is -> Emph (normalize i) <| normalize is
    Strong i :< is -> Strong (normalize i) <| normalize is
    Task id' s i :< is -> Task id' s (normalize i) <| normalize is
    Str t :< is -> Str t <| normalize is
    Code t :< is -> Code t <| normalize is
    HardBreak :< is -> HardBreak <| normalize is
    SoftBreak :< is -> SoftBreak <| normalize is
    EmptyL -> mempty


-- | Extract textual content from an inline.
--   Note that it extracts only the 'primary' content (the one that is shown in
--   first place). For example it wouldn't extract an URL from the link.
asText :: Inline id -> Text
asText = \case
    Str    t           -> t
    Emph   is          -> foldMap asText is
    Strong is          -> foldMap asText is
    Code   t           -> t
    Link  is _ _       -> foldMap asText is
    Image is _ _       -> foldMap asText is
    SoftBreak          -> " "
    HardBreak          -> "\n"
    Task _id status is -> renderStatus status <> " " <> foldMap asText is
  where
    renderStatus :: IsString a => TaskStatus -> a
    renderStatus = \case
        Todo -> fromString "[ ]"
        Done -> fromString "[x]"
