{-# LANGUAGE TemplateHaskell #-}

module Frontend.Static where

import Obelisk.Generated.Static (static)
import Reflex.Dom.Core
import Relude

includeAssets :: DomBuilder t m => m ()
includeAssets = do
  elAttr "link" ("href" =: $(static "main-compiled.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
