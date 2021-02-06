{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Common.Api (EmanoteApi (..))
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
      widgetHold_ (text "Loading...") $
        ffor resp $ \case
          Left (err :: Text) -> text (show err)
          Right notes ->
            el "ul" $ do
              forM_ notes $ \wId -> do
                el "li" $ do
                  routeLink (FrontendRoute_Note :/ wId) $ text $ untag wId
    FrontendRoute_Note -> do
      routeLink (FrontendRoute_Main :/ ()) $ text "Back to /"
      el "h1" $ do
        r <- askRoute
        dynText $ untag <$> r
      req <- fmap EmanoteApi_Note <$> askRoute
      resp <- App.requestingDynamic req
      widgetHold_ (text "Loading...") $
        ffor resp $ \case
          Left err -> text (show err)
          Right Nothing -> text "No such note"
          Right (Just (Left conflict)) -> text (show conflict)
          Right (Just (Right (_fp, Left parseErr))) -> text (show parseErr)
          Right (Just (Right (_fp, Right doc))) -> do
            let cfg =
                  PR.defaultConfig
                    { PR._config_renderLink = linkRender
                    }
            divClass "bg-gray-100 rounded-xl" $ do
              PR.elPandoc cfg doc
  where
    linkRender defRender url attrs _minner =
      fromMaybe defRender $ do
        (lbl, wId) <- parseWikiLinkUrl (Map.lookup "title" attrs) url
        pure $ do
          let r = constDyn $ FrontendRoute_Note :/ wId
              attr = constDyn $ "title" =: show lbl
          routeLinkDynAttr attr r $ do
            text $ untag wId
