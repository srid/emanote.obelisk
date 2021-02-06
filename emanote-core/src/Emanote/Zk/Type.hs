{-# LANGUAGE DataKinds #-}

module Emanote.Zk.Type where

import Data.Conflict (Conflict)
import Data.Tagged (Tagged)
import Relude
import Text.Pandoc.Definition (Pandoc)

type ParserError = Tagged "ParserError" Text

type Zettel =
  Either
    -- More than one file uses the same ID.
    (Conflict FilePath ByteString)
    ( -- Path on disk
      FilePath,
      Either
        -- Failed to parse markdown
        ParserError
        -- Pandoc AST
        Pandoc
    )
