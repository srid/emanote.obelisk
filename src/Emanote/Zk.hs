module Emanote.Zk where

import Data.Conflict (Conflict)
import Emanote.Graph.Patch (PatchGraph)
import qualified Emanote.Markdown as M
import qualified Emanote.Markdown.WikiLink as M
import Reflex (PatchMap)
import Reflex.TIncremental (TIncremental)
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
  { _zk_zettels :: TIncremental (PatchMap M.WikiLinkID Zettel),
    _zk_graph :: TIncremental PatchGraph,
    _zk_htmlTemplate :: TIncremental (PatchMap FilePath (Either Text Mustache.Template))
  }
