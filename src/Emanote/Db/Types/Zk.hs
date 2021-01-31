{-# LANGUAGE TypeFamilies #-}

module Emanote.Db.Types.Zk where

import Data.Conflict (Conflict)
import Data.Default (Default (..))
import Emanote.Graph (Graph)
import qualified Emanote.Graph as Graph
import qualified Emanote.Markdown as M
import qualified Emanote.Markdown.WikiLink as M
import qualified Text.Mustache.Types as Mustache
import Text.Pandoc.Definition (Pandoc)

type Zettel =
  Either
    -- More than one file uses the same ID.
    (Conflict FilePath ByteString)
    ( -- Path on disk
      FilePath,
      Either
        -- Failed to parse markdown
        M.ParserError
        -- Pandoc AST
        Pandoc
    )

data Zk = Zk
  { _zk_zettels :: Map M.WikiLinkID Zettel,
    _zk_graph :: Graph,
    _zk_htmlTemplate :: Map FilePath (Either Text Mustache.Template)
  }

instance Default Zk where
  def = Zk mempty Graph.empty mempty
