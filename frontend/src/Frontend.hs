{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Common.Api
import Common.Route
import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import Data.Tagged
import Emanote.Markdown.WikiLink
import qualified Frontend.App as App
import Obelisk.Frontend
import Obelisk.Generated.Static (static)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import qualified Reflex.Dom.Pandoc as PR
import Relude

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = do
        elAttr "meta" ("content" =: "text/html; charset=utf-8" <> "http-equiv" =: "Content-Type") blank
        elAttr "meta" ("content" =: "width=device-width, initial-scale=1" <> "name" =: "viewport") blank
        el "title" $ text "Emanote"
        elAttr "link" ("href" =: static @"main-compiled.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank,
      _frontend_body = do
        divClass "min-h-screen bg-gray-100 md:container mx-auto px-4" $ do
          App.runApp app
    }

app ::
  forall t m js.
  ( DomBuilder t m,
    MonadHold t m,
    PostBuild t m,
    MonadFix m,
    Prerender js t m,
    RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    Response m ~ Either Text,
    Request m ~ EmanoteApi,
    Requester t m
  ) =>
  RoutedT t (R FrontendRoute) m ()
app = do
  subRoute_ $ \case
    FrontendRoute_Main -> do
      el "h1" $ text "Emanote"
      req <- fmap (const EmanoteApi_GetNotes) <$> askRoute
      resp <- App.requestingDynamic req
      widgetHold_ loader $
        ffor resp $ \case
          Left (err :: Text) -> text (show err)
          Right notes -> do
            el "ul" $ do
              forM_ notes $ \wId -> do
                el "li" $ do
                  routeLink (FrontendRoute_Note :/ wId) $ text $ untag wId
    FrontendRoute_Note -> do
      req <- fmap EmanoteApi_Note <$> askRoute
      resp <- App.requestingDynamic req
      widgetHold_ loader $
        ffor resp $ \case
          Left err -> text (show err)
          Right (note :: Note) -> do
            divClass "grid gap-4 grid-cols-6" $ do
              divClass "col-start-1 col-span-2" $ do
                divClass "linksBox" $ do
                  routeLink (FrontendRoute_Main :/ ()) $ text "Back to /"
                divClass "linksBox" $ do
                  el "h2" $ text "Uplinks"
                  elClass "ul" "uplinks " $ do
                    forM_ (_note_uplinks note) $ \LinkContext {..} -> do
                      el "li" $ do
                        iconBack
                        routeLink (FrontendRoute_Note :/ _linkcontext_id) $ text $ untag _linkcontext_id
                        divClass "opacity-50 hover:opacity-100 text-sm" $ do
                          renderPandoc _linkcontext_ctx
                divClass "linksBox" $ do
                  el "h2" $ text "Backlinks"
                  elClass "ul" "backlinks " $ do
                    forM_ (_note_backlinks note) $ \LinkContext {..} -> do
                      el "li" $ do
                        routeLink (FrontendRoute_Note :/ _linkcontext_id) $ text $ untag _linkcontext_id
                        divClass "opacity-50 hover:opacity-100 text-sm" $ do
                          renderPandoc _linkcontext_ctx

              divClass "col-start-3 col-span-4" $ do
                el "h1" $ do
                  r <- askRoute
                  dynText $ untag <$> r
                case _note_zettel note of
                  Nothing -> text "No such note"
                  Just z ->
                    case z of
                      Left conflict -> text (show conflict)
                      Right (_fp, Left parseErr) -> text (show parseErr)
                      Right (_fp, Right doc) -> do
                        divClass "bg-gray-100 rounded-xl" $ do
                          renderPandoc doc
                divClass "" $ do
                  divClass "linksBox" $ do
                    el "h2" $ text "Downlinks"
                    elClass "ul" "downlinks " $ do
                      forM_ (_note_downlinks note) $ \LinkContext {..} -> do
                        el "li" $ do
                          routeLink (FrontendRoute_Note :/ _linkcontext_id) $ text $ untag _linkcontext_id
                          divClass "opacity-50 hover:opacity-100 text-sm" $ do
                            renderPandoc _linkcontext_ctx
                  divClass "linksBox" $ do
                    el "h2" $ text "Orphans"
                    elClass "ul" "orphans " $ do
                      forM_ (_note_orphans note) $ \LinkContext {..} -> do
                        el "li" $ do
                          routeLink (FrontendRoute_Note :/ _linkcontext_id) $ text $ untag _linkcontext_id
  where
    -- FIXME: doesn't work
    iconBack :: DomBuilder t m1 => m1 ()
    iconBack = do
      elAttr
        "svg"
        ( "xmlns" =: "http://www.w3.org/2000/svg"
            <> "fill" =: "none"
            <> "viewBox" =: "0 0 24 24"
            <> "stroke" =: "currentColor"
        )
        $ do
          elAttr
            "path"
            ( "stroke-linecap" =: "round"
                <> "stroke-linejoin" =: "round"
                <> "stroke-width" =: "2"
                <> "d" =: "M11 15l-3-3m0 0l3-3m-3 3h8M3 12a9 9 0 1118 0 9 9 0 01-18 0z"
            )
            blank
    renderPandoc doc = do
      let cfg =
            PR.defaultConfig
              { PR._config_renderLink = linkRender
              }
      PR.elPandoc cfg doc
    linkRender defRender url attrs _minner =
      fromMaybe defRender $ do
        (lbl, wId) <- parseWikiLinkUrl (Map.lookup "title" attrs) url
        pure $ do
          let r = constDyn $ FrontendRoute_Note :/ wId
              attr = constDyn $ "title" =: show lbl
          routeLinkDynAttr attr r $ do
            text $ untag wId

loader :: DomBuilder t m => m ()
loader = do
  text "Loading..."
