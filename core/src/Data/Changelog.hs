{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Keep a Changelog data structure and commonmark rendering.
--
-- See <https://keepachangelog.com/>.
module Data.Changelog
  ( -- * Types
    Changelog (Changelog),
    ChangeType (Added, Changed, Deprecated, Fixed, Removed, Security),
    Release (Release),
    Sections,
    URL,
    Unreleased (Unreleased),
    VersioningSystem (Other, PVP, SemVer),

    -- * Rendering
    render,
    renderUnreleased,
    renderRelease,
    textItem,
    text,

    -- * Parsing
    ParseError (InvalidDate, InvalidReleaseHeading, NoReleases, NotADocument),
    fromNode,
    parse,
  )
where

import safe "base" Control.Applicative (empty, (<|>))
import safe qualified "base" Control.Applicative as Ap
import safe "base" Control.Category ((.))
import safe "base" Data.Bifunctor (first)
import safe "base" Data.Bool (Bool (False, True), bool, not)
import safe "base" Data.Either (Either (Left, Right))
import safe "base" Data.Eq (Eq)
import safe "base" Data.Foldable (Foldable, foldMap, foldl', foldr)
import safe "base" Data.Function (const, ($))
import safe "base" Data.Functor (Functor, (<$>))
import safe "base" Data.Kind (Type)
import safe "base" Data.List (span, unfoldr)
import safe "base" Data.List.NonEmpty (NonEmpty, nonEmpty)
import safe "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import safe "base" Data.Ord (Ord)
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.Traversable (Traversable)
import safe "base" Data.Tuple (snd, uncurry)
import safe "base" GHC.Generics (Generic, Generic1)
import safe "base" Text.Read (Read)
import safe "base" Text.Show (Show)
import safe "cmark" CMark
  ( DelimType (PERIOD_DELIM),
    ListAttributes (ListAttributes),
    ListType (BULLET_LIST),
    Node (Node),
    NodeType (DOCUMENT, HEADING, ITEM, LINK, LIST, PARAGRAPH, TEXT),
    commonmarkToNode,
    nodeToCommonmark,
  )
import safe "containers" Data.Map.Strict (Map)
import safe qualified "containers" Data.Map.Strict as Map
-- WAIT: Can remove `*Commutative` once there’s a new release of duoids that
--       includes instances for `Maybe`, etc.
import safe "duoids" Control.Duoidal
  ( Commutative (Commutative),
    getCommutative,
    pure,
    traverse,
    (<=<),
  )
import safe "text" Data.Text (Text)
import safe qualified "text" Data.Text as T
import safe "time" Data.Time.Calendar.OrdinalDate (Day)
import safe "time" Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import safe "base" Prelude (Bounded, Enum)

-- | A URL.
--
-- @since 0.0.1
type URL = Text :: Type

-- | The versioning system a project adheres to.
--
-- @since 0.0.1
data VersioningSystem
  = -- | <https://semver.org/ Semantic Versioning>, carrying the spec version
    --   (e.g. @"2.0.0"@).
    SemVer Text
  | -- | The <https://pvp.haskell.org/ Haskell Package Versioning Policy>.
    PVP
  | -- | A custom versioning system with a name and optional URL.
    Other Text (Maybe URL)
  deriving (Eq, Generic, Ord, Read, Show)

-- | A changelog following the [Keep a Changelog](https://keepachangelog.com/)
--   format.
--
-- @since 0.0.1
data Changelog (item :: Type) = Changelog
  { versioningSystem :: Maybe VersioningSystem,
    unreleased :: Maybe (Unreleased item),
    releases :: NonEmpty (Release item)
  }
  deriving
    ( Eq,
      Generic,
      Ord,
      Read,
      Show,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )

type role Changelog representational

-- | An unreleased section with its URL and change sections.
--
-- @since 0.0.1
data Unreleased (item :: Type) = Unreleased
  { url :: URL,
    sections :: Sections item
  }
  deriving
    ( Eq,
      Generic,
      Ord,
      Read,
      Show,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )

type role Unreleased representational

-- | A single release entry with its version, URL, and change sections.
--
-- @since 0.0.1
data Release (item :: Type) = Release
  { version :: Text,
    url :: URL,
    date :: Day,
    yanked :: Bool,
    sections :: Sections item
  }
  deriving
    ( Eq,
      Generic,
      Ord,
      Read,
      Show,
      Foldable,
      Functor,
      Generic1,
      Traversable
    )

type role Release representational

-- | Categories of changes per the Keep a Changelog specification.
--
-- @since 0.0.1
data ChangeType
  = Added
  | Changed
  | Deprecated
  | Fixed
  | Removed
  | Security
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

-- | Change sections, mapping each `ChangeType` to its non-empty list of items.
--
--   Each @item@ is the block-level content of one list item.
--
-- @since 0.0.1
type Sections (item :: Type) = Map ChangeType (NonEmpty item) :: Type

-- | Errors that can occur when parsing a changelog from a cmark `Node` tree.
--
-- @since 0.0.1
data ParseError
  = -- | The root node is not a `DOCUMENT`.
    NotADocument
  | -- | No release entries (@`HEADING` 2@) were found.
    NoReleases
  | -- | A @`HEADING` 2@ could not be parsed as a release heading.
    InvalidReleaseHeading [Node]
  | -- | A date string could not be parsed as YYYY-MM-DD.
    InvalidDate Text
  deriving (Eq, Generic, Ord, Read, Show)

-- | Render a `Changelog` to Markdown suitable for a CHANGELOG.md file.
--
-- @since 0.0.1
render :: Changelog Node -> Text
render (Changelog mVs mUnrel rs) =
  "# Changelog\n"
    <> foldMap renderPreamble mVs
    <> foldMap renderUnreleased' mUnrel
    <> foldMap (("\n" <>) . renderRelease') rs
    <> renderFooter mUnrel rs

renderPreamble :: VersioningSystem -> Text
renderPreamble vs =
  nodeToCommonmark [] empty (Node empty DOCUMENT (preambleNodes vs))

-- Preamble

preambleNodes :: VersioningSystem -> [Node]
preambleNodes vs =
  [ Node empty PARAGRAPH . Ap.pure $
      text
        "All notable changes to this project will be documented in this file.",
    Node empty PARAGRAPH $
      [ text "The format is based on ",
        Node
          empty
          (LINK "https://keepachangelog.com/en/1.1.0/" "")
          [text "Keep a Changelog"],
        text ", and this project adheres to ",
        versioningRef vs,
        text "."
      ]
  ]

versioningRef :: VersioningSystem -> Node
versioningRef = \case
  SemVer ver ->
    Node
      empty
      (LINK ("https://semver.org/spec/v" <> ver <> ".html") "")
      [text "Semantic Versioning"]
  PVP ->
    Node
      empty
      (LINK "https://pvp.haskell.org/" "")
      [text "the Haskell Package Versioning Policy"]
  Other name mUrl ->
    foldr
      (\u -> Node empty (LINK u "") . Ap.pure)
      (text name)
      mUrl

renderUnreleased' :: Unreleased Node -> Text
renderUnreleased' (Unreleased _url secs) =
  "## [Unreleased]\n\n"
    <> nodeToCommonmark [] empty (Node empty DOCUMENT (sectionsNodes secs))

-- | Render an `Unreleased` entry with its footer.
--
-- @since 0.0.1
renderUnreleased :: Unreleased Node -> Text
renderUnreleased unreleased =
  renderUnreleased' unreleased <> "\n" <> unrelLinkDef unreleased

-- Releases

renderRelease' :: Release Node -> Text
renderRelease' (Release v _url d yanked secs) =
  -- TODO: This doesn’t use cmark here because it will print an inline link,
  --       rather than using a link reference. And the reference is both clearer
  --       and implied by Keep a Changelog.
  "## ["
    <> v
    <> "] - "
    <> renderDate d
    <> bool "" " [YANKED]" yanked
    <> "\n\n"
    <> nodeToCommonmark [] empty (Node empty DOCUMENT (sectionsNodes secs))

-- | Render a single `Release` entry with its footer.
--
-- @since 0.0.1
renderRelease :: Release Node -> Text
renderRelease release =
  renderRelease' release <> "\n" <> relLinkDef release

-- Footer

renderFooter :: Maybe (Unreleased Node) -> NonEmpty (Release Node) -> Text
renderFooter mUnrel rs =
  "\n" <> foldMap unrelLinkDef mUnrel <> foldMap relLinkDef rs

unrelLinkDef :: Unreleased Node -> Text
unrelLinkDef (Unreleased url _) = "[Unreleased]: " <> url <> "\n"

relLinkDef :: Release Node -> Text
relLinkDef (Release v url _ _ _) = "[" <> v <> "]: " <> url <> "\n"

-- Sections

sectionsNodes :: Sections Node -> [Node]
sectionsNodes = Map.foldMapWithKey sectionNodes

sectionNodes :: ChangeType -> NonEmpty Node -> [Node]
sectionNodes ct items =
  [ Node empty (HEADING 3) . Ap.pure . text $ changeTypeLabel ct,
    Node empty (LIST $ ListAttributes BULLET_LIST True 0 PERIOD_DELIM) $
      foldMap (Ap.pure . itemNode) items
  ]

changeTypeLabel :: ChangeType -> Text
changeTypeLabel = \case
  Added -> "Added"
  Changed -> "Changed"
  Deprecated -> "Deprecated"
  Fixed -> "Fixed"
  Removed -> "Removed"
  Security -> "Security"

itemNode :: Node -> Node
itemNode n = Node empty ITEM [n]

-- Helpers

renderDate :: Day -> Text
renderDate = T.pack . iso8601Show

-- | Parse Markdown text into a block-level `Node` suitable for use as a
--   `Section` item.
--
--   This can be used to convert the `Changelog` before rendering.
--
-- > render :: ChangeLog Node -> Text
-- > render . fmap textItem :: ChangeLog Text -> Text
--
-- @since 0.0.1
textItem :: Text -> Node
textItem t = case commonmarkToNode [] t of
  Node _ DOCUMENT [p] -> p
  doc -> doc

-- | Text to use literally as a `Section` item (Markdown will be escaped).
--
--   This can be used to convert the `Changelog` before rendering.
--
-- > render :: ChangeLog Node -> Text
-- > render . fmap text :: ChangeLog Text -> Text
--
-- @since 0.0.1
text :: Text -> Node
text t = Node empty (TEXT t) []

-- Parsing

-- | Parse commonmark text into a `Changelog`.
--
-- @since 0.0.1
parse :: Text -> Either (NonEmpty ParseError) (Changelog Node)
parse = fromNode . commonmarkToNode []

-- | Extract a `Changelog` from a cmark document `Node`.
--
-- @since 0.0.1
fromNode :: Node -> Either (NonEmpty ParseError) (Changelog Node)
fromNode = \case
  Node _ DOCUMENT children ->
    let (preamble, groups) = splitAtH2 children
        vs = extractVersioningSystem preamble
     in case groups of
          (hd, body) : rest ->
            maybe
              (Changelog vs Nothing <$> extractReleases groups)
              ( \url ->
                  Changelog
                    vs
                    (getCommutative . pure . Unreleased url $ parseSections body)
                    <$> extractReleases rest
              )
              $ extractUnreleasedHeading hd
          [] -> Left $ Ap.pure NoReleases
  Node _ _ _ -> Left $ Ap.pure NotADocument

extractReleases ::
  [([Node], [Node])] -> Either (NonEmpty ParseError) (NonEmpty (Release Node))
extractReleases =
  note (Ap.pure NoReleases)
    . nonEmpty
    <=< traverse (first Ap.pure . uncurry extractRelease)

extractRelease :: [Node] -> [Node] -> Either ParseError (Release Node)
extractRelease hd body =
  (\(v, url, d, yanked) -> Release v url d yanked $ parseSections body)
    <$> parseReleaseHeading hd

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

-- Split document children at HEADING 2 boundaries.
splitAtH2 :: [Node] -> ([Node], [([Node], [Node])])
splitAtH2 nodes =
  let (preamble, rest) = span (not . isH2) nodes
   in (preamble, groupH2s rest)

isH2 :: Node -> Bool
isH2 (Node _ (HEADING 2) _) = True
isH2 _ = False

groupH2s :: [Node] -> [([Node], [Node])]
groupH2s = unfoldr stepH2

stepH2 :: [Node] -> Maybe (([Node], [Node]), [Node])
stepH2 = \case
  (Node _ (HEADING 2) inlines : rest) ->
    let (body, remaining) = span (not . isH2) rest
     in getCommutative $ pure ((inlines, body), remaining)
  _ -> empty

-- Unreleased heading: HEADING 2 [LINK url "" [TEXT "Unreleased"]]
extractUnreleasedHeading :: [Node] -> Maybe URL
extractUnreleasedHeading = \case
  [Node _ (LINK url _) [Node _ (TEXT "Unreleased") _]] ->
    getCommutative $ pure url
  _ -> empty

-- Release heading: HEADING 2 [LINK url [TEXT ver], TEXT " - date", ...]
parseReleaseHeading :: [Node] -> Either ParseError (Text, URL, Day, Bool)
parseReleaseHeading = \case
  Node _ (LINK url _) [Node _ (TEXT ver) _] : rest ->
    (\(d, yanked) -> (ver, url, d, yanked)) <$> parseDateAndYanked rest
  heading -> Left $ InvalidReleaseHeading heading

parseDateAndYanked :: [Node] -> Either ParseError (Day, Bool)
parseDateAndYanked dateAndYanked = case dateAndYanked of
  Node _ (TEXT dateStr) _ : _ -> do
    raw <-
      note (InvalidReleaseHeading dateAndYanked) $ T.stripPrefix " - " dateStr
    maybe
      (note (InvalidDate raw) $ (,False) <$> parseDate raw)
      ( \dateOnly ->
          note (InvalidDate dateOnly) $ (,True) <$> parseDate dateOnly
      )
      $ T.stripSuffix " [YANKED]" raw
  _ -> Left $ InvalidReleaseHeading dateAndYanked

-- Sections: pairs of HEADING 3 + LIST
parseSections :: [Node] -> Sections Node
parseSections = Map.fromList . foldMap parseH3Pair . pairH3List

pairH3List :: [Node] -> [(Text, Node)]
pairH3List =
  snd . foldl' step (empty, [])
  where
    step (Just label, acc) list@(Node _ (LIST _) _) = (empty, acc <> [(label, list)])
    step (_, acc) (Node _ (HEADING 3) [Node _ (TEXT label) _]) = (getCommutative $ pure label, acc)
    step (_, acc) _ = (empty, acc)

parseH3Pair :: (Text, Node) -> [(ChangeType, NonEmpty Node)]
parseH3Pair (label, listNode) =
  foldMap (\ct -> foldMap (Ap.pure . (ct,)) $ extractListItems listNode) $
    parseChangeType label

extractListItems :: Node -> Maybe (NonEmpty Node)
extractListItems = \case
  Node _ (LIST _) items -> nonEmpty (foldMap itemContent items)
  Node _ _ _ -> Nothing

itemContent :: Node -> [Node]
itemContent = \case
  Node _ ITEM (child : _) -> [child]
  Node _ _ _ -> []

parseChangeType :: Text -> Maybe ChangeType
parseChangeType =
  getCommutative . \case
    "Added" -> pure Added
    "Changed" -> pure Changed
    "Deprecated" -> pure Deprecated
    "Fixed" -> pure Fixed
    "Removed" -> pure Removed
    "Security" -> pure Security
    _ -> Commutative empty

-- Preamble: extract versioning system from paragraphs before first H2
extractVersioningSystem :: [Node] -> Maybe VersioningSystem
extractVersioningSystem = foldr ((<|>) . findVSInParagraph) empty

findVSInParagraph :: Node -> Maybe VersioningSystem
findVSInParagraph (Node _ PARAGRAPH children) = findVSLink children
findVSInParagraph _ = empty

findVSLink :: [Node] -> Maybe VersioningSystem
findVSLink =
  snd . foldl' step (False, empty)
  where
    step (_, acc@(Just _)) _ = (False, acc)
    step (True, _) (Node _ (LINK url _) [Node _ (TEXT name) _]) =
      (False, getCommutative $ pure (classifyVS name url))
    step (_, _) (Node _ (TEXT t) _) = ("adheres to " `T.isSuffixOf` t, empty)
    step (_, _) _ = (False, empty)

classifyVS :: Text -> URL -> VersioningSystem
classifyVS = \case
  "Semantic Versioning" -> SemVer . extractSemVerVersion
  "the Haskell Package Versioning Policy" -> const PVP
  name -> Other name . getCommutative . pure

extractSemVerVersion :: URL -> Text
extractSemVerVersion =
  -- Extract version from "https://semver.org/spec/vX.Y.Z.html"
  maybe
    ""
    ( \rest -> case T.splitOn ".html" rest of
        ver : _ -> ver
        [] -> ""
    )
    . T.stripPrefix "https://semver.org/spec/v"

parseDate :: Text -> Maybe Day
parseDate = iso8601ParseM . T.unpack
