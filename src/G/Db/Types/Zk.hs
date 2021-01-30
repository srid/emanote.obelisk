{-# LANGUAGE TypeFamilies #-}

module G.Db.Types.Zk where

import Data.Conflict (Conflict)
import Data.Default (Default (..))
import G.Graph (Graph)
import qualified G.Graph as Graph
import qualified G.Markdown as M
import qualified G.Markdown.WikiLink as M
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
    _zk_graph :: Graph
  }

instance Default Zk where
  def = Zk mempty Graph.empty
