{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Emanote.Markdown.WikiLink
where

import qualified Commonmark as CM
import qualified Commonmark.Inlines as CM
import Commonmark.TokParsers (noneOfToks, symbol)
import Data.Tagged (Tagged (..), untag)
import qualified Data.Text as T
import qualified Network.URI.Encode as URIEncode
import qualified Text.Megaparsec as M
import Text.Pandoc.Definition (Block)
import qualified Text.Parsec as P
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
  deriving (Eq, Ord)

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
