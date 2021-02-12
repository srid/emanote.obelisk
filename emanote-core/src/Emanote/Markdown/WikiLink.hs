{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Emanote.Markdown.WikiLink where

import Data.Aeson
import Data.Tagged (Tagged (..))
import qualified Data.Text as T
import qualified Network.URI.Encode as URIEncode
import Relude
import Text.Pandoc.Definition
import Text.Read
import qualified Text.Show (Show (..))

-- | The inner text of a wiki link.
type WikiLinkID = Tagged "WikiLinkID" Text

-- | Make [[Foo]] link to "Foo". In future, make this configurable.
renderWikiLinkUrl :: WikiLinkID -> Text
renderWikiLinkUrl (Tagged s) = toText $ URIEncode.encode $ toString s

-- | Parse what was rendered by renderWikiLinkUrl
parseWikiLinkUrl :: Maybe Text -> Text -> Maybe (WikiLinkLabel, WikiLinkID)
parseWikiLinkUrl mtitle s = do
  guard $ not $ ":" `T.isInfixOf` s
  guard $ not $ "/" `T.isInfixOf` s
  let linkLabel = parseWikiLinkLabel mtitle
      linkId = Tagged $ toText $ URIEncode.decode (toString s)
  pure (linkLabel, linkId)

data WikiLinkLabel
  = WikiLinkLabel_Unlabelled
  | -- | [[Foo]]#
    WikiLinkLabel_Branch
  | -- | #[[Foo]]
    WikiLinkLabel_Tag
  deriving (Eq, Ord, Generic, ToJSON, FromJSON)

instance Semigroup WikiLinkLabel where
  WikiLinkLabel_Unlabelled <> x = x
  x <> WikiLinkLabel_Unlabelled = x
  WikiLinkLabel_Tag <> _ = WikiLinkLabel_Tag
  _ <> WikiLinkLabel_Tag = WikiLinkLabel_Tag
  WikiLinkLabel_Branch <> WikiLinkLabel_Branch = WikiLinkLabel_Branch

-- | The AST "surrounding" a wiki-link (any link, in fact)
type WikiLinkContext = [Block]

-- Show value is stored in the `title` attribute of the <a> element (of Pandoc
-- AST), and then retrieved later using the Read instance further below. This is
-- how we store link labels in the Pandoc AST.
instance Show WikiLinkLabel where
  show = \case
    WikiLinkLabel_Unlabelled -> "link:nolbl"
    WikiLinkLabel_Branch -> "link:branch"
    WikiLinkLabel_Tag -> "link:tag"

instance Read WikiLinkLabel where
  readsPrec _ s
    | s == show WikiLinkLabel_Unlabelled =
      [(WikiLinkLabel_Unlabelled, "")]
    | s == show WikiLinkLabel_Branch =
      [(WikiLinkLabel_Branch, "")]
    | s == show WikiLinkLabel_Tag =
      [(WikiLinkLabel_Tag, "")]
    | otherwise = []

-- | Represent a Wiki link without the target (WikiLinkID)
data WikiLink = WikiLink
  { _wikilink_label :: WikiLinkLabel,
    _wikilink_ctx :: Maybe WikiLinkContext
  }
  deriving (Eq, Ord, Generic, ToJSON, FromJSON)

-- Just for debugging
instance Show WikiLink where
  show (WikiLink lbl mctx) =
    "WikiLink[" <> show lbl <> "][ctx:" <> bool "no" "yes" (isJust mctx)

mkWikiLink :: WikiLinkLabel -> WikiLinkContext -> WikiLink
mkWikiLink lbl ctx =
  WikiLink lbl $ guard (not $ singleLinkContext ctx) >> pure ctx
  where
    -- A link context that has nothing but a single wiki-link
    singleLinkContext = \case
      -- Wiki-link on its own line (paragraph)
      [Para [Link _ _ (_, linkTitle)]] | "link:" `T.isPrefixOf` linkTitle -> True
      -- Wiki-link on its own in a list item
      [Plain [Link _ _ (_, linkTitle)]] | "link:" `T.isPrefixOf` linkTitle -> True
      _ -> False

renderWikiLinkLabel :: WikiLinkLabel -> Text
renderWikiLinkLabel = show

-- | Determine label from the optional "title" attribute
parseWikiLinkLabel :: Maybe Text -> WikiLinkLabel
parseWikiLinkLabel mtitle =
  fromMaybe WikiLinkLabel_Unlabelled $
    readMaybe . toString =<< mtitle

data Directed a
  = -- | In the same direction as the user linked to.
    UserDefinedDirection a
  | -- | In the reverse direction to the one user linked to.
    ReverseDirection a
  deriving (Eq, Ord, Show, Functor)

isReverse :: Directed a -> Bool
isReverse = \case
  ReverseDirection _ -> True
  _ -> False

isParent :: Directed WikiLinkLabel -> Bool
isParent = \case
  UserDefinedDirection WikiLinkLabel_Tag -> True
  ReverseDirection WikiLinkLabel_Branch -> True
  _ -> False

isBranch :: Directed WikiLinkLabel -> Bool
isBranch = \case
  UserDefinedDirection WikiLinkLabel_Branch -> True
  ReverseDirection WikiLinkLabel_Tag -> True
  _ -> False

-- The three abstract kinds of links.

-- | Is this link a (non-folgezettel) backlink?
isBacklink :: Directed WikiLinkLabel -> Bool
isBacklink l =
  isReverse l
    && not (isParent l)
    && not (isBranch l)

-- | Is this link from a note that tags self?
isTaggedBy :: Directed WikiLinkLabel -> Bool
isTaggedBy l =
  isReverse l
    && isBranch l

isUplink :: Directed WikiLinkLabel -> Bool
isUplink =
  isParent
