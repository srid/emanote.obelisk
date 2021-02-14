{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Frontend.Widget where

import Common.Route
import Data.Tagged
import Emanote.Markdown.WikiLink
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Relude hiding (on)

wikiLinkAttrs :: Map AttributeName Text
wikiLinkAttrs =
  "class" =: "text-green-700"

renderWikiLink ::
  ( PostBuild t m,
    RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    Prerender js t m,
    DomBuilder t m
  ) =>
  Map AttributeName Text ->
  Dynamic t WikiLinkLabel ->
  Dynamic t WikiLinkID ->
  m ()
renderWikiLink attrs lbl wId =
  routeLinkDynAttr
    ( ffor lbl $ \x ->
        "title" =: show x <> attrs
    )
    ( ffor wId $ \x ->
        FrontendRoute_Note :/ x
    )
    $ dynText $ untag <$> wId
