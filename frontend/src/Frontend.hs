{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Common.Api (EmanoteApi (..), EmanoteNet)
import Common.Route
import Control.Monad.Fix (MonadFix)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Constraint.Extras (Has)
import qualified Data.Text as T
import Obelisk.Configs (getTextConfig)
import Obelisk.Frontend
import Obelisk.Generated.Static (static)
import Obelisk.Route
import Reflex.Dom.Core
import Reflex.Dom.GadtApi
import Relude

type ValidEnc = Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = do
        el "title" $ text "Emanote"
        elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank,
      _frontend_body = do
        el "h1" $ text "Emanote"
        let enc :: Either Text ValidEnc = checkEncoder fullRouteEncoder
        r <- getTextConfig "common/route"
        case (enc, r) of
          (Left _, _) -> error "Routes are invalid!"
          (_, Nothing) -> error "Couldn't load common/route config file"
          (Right validEnc, Just host) -> do
            startEmanoteNet $
              Right $
                T.replace "http" "ws" host
                  <> renderBackendRoute validEnc (BackendRoute_WebSocket :/ ())
    }

startEmanoteNet ::
  forall js t m.
  ( Prerender js t m,
    MonadHold t m,
    MonadIO (Performable m),
    MonadFix m,
    DomBuilder t m,
    PostBuild t m,
    TriggerEvent t m,
    PerformEvent t m,
    Has FromJSON EmanoteApi,
    forall a. ToJSON (EmanoteApi a)
  ) =>
  Either ApiEndpoint WebSocketEndpoint ->
  m ()
startEmanoteNet endpoint = do
  rec (_, requests) <- runRequesterT start responses
      responses <- case endpoint of
        Left xhr -> performXhrRequests xhr (requests :: Event t (RequesterData EmanoteApi))
        Right ws -> performWebSocketRequests ws (requests :: Event t (RequesterData EmanoteApi))
  pure ()
  where
    start :: EmanoteNet t m ()
    start = do
      go <- button "Go!"
      resp <-
        requestingJs $
          EmanoteApi_GetNotes <$ go
      widgetHold_ (text "...") $
        ffor resp $ \case
          Left err -> text (show err)
          Right notes ->
            el "tt" $ text (show notes)
      pure ()

requestingJs ::
  (Reflex t, MonadFix m, Prerender js t m) =>
  Event t (Request (Client (EmanoteNet t m)) a) ->
  EmanoteNet
    t
    m
    (Event t (Response (Client (EmanoteNet t m)) a))
requestingJs
  r = fmap (switch . current) $ prerender (pure never) $ requesting r
