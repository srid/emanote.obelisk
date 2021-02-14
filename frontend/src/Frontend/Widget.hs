{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Frontend.Widget where

import Common.Route
import Data.Tagged
import Emanote.Markdown.WikiLink
import "ghcjs-dom" GHCJS.DOM.Document (getBodyUnchecked)
import GHCJS.DOM.EventM (on) --  preventDefault)
import GHCJS.DOM.GlobalEventHandlers (keyUp)
import Language.Javascript.JSaddle.Types (MonadJSM)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core hiding (Link, preventDefault)
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

captureKey ::
  ( DomBuilder t m,
    HasDocument m,
    TriggerEvent t m,
    DomBuilderSpace m ~ GhcjsDomSpace,
    MonadJSM m
  ) =>
  Key ->
  m (Event t Key)
captureKey key = do
  doc <- askDocument
  body <- getBodyUnchecked doc
  kp <- wrapDomEvent body (`on` keyUp) $ do
    keyEvent <- getKeyEvent
    let keyPressed = keyCodeLookup (fromEnum keyEvent)
    -- Use 'preventDefault' here to prevent the browser's default behavior
    -- when keys like <F1> or the arrow keys are pressed. If you want to
    -- preserve default behavior don't use it, or you can apply it
    -- selectively, only to certain keypresses.
    if keyPressed == key
      then pure (Just keyPressed)
      else pure Nothing
  pure $ fforMaybe kp id
