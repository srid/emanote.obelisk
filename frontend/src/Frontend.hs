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
import qualified Frontend.Static as Static
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import qualified Reflex.Dom.Pandoc as PR
import Relude

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute) (Maybe Text)
frontend =
  Frontend
    { _frontend_head = \titDyn -> do
        elAttr "meta" ("content" =: "text/html; charset=utf-8" <> "http-equiv" =: "Content-Type") blank
        elAttr "meta" ("content" =: "width=device-width, initial-scale=1" <> "name" =: "viewport") blank
        el "title" $ do
          dynText $ fromMaybe "Home" <$> titDyn
          text " | Emanote"
        Static.includeAssets,
      _frontend_body = do
        divClass "min-h-screen md:container mx-auto px-4" $ do
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
  RoutedT t (R FrontendRoute) m (Dynamic t (Maybe Text))
app = do
  fmap join $
    subRoute $ \case
      FrontendRoute_Main -> do
        divClass "grid gap-4 grid-cols-3" $ do
          divClass "col-start-2" $ do
            el "h1" $ text "Emanote"
            elClass "p" "rounded border-2 mt-2 mb-2 p-2" $
              text "Welcome to Emanote. This place will soon look like a search engine, allowing you to query your notebook graph. For now, we simply display the list of notes."
            req <- fmap (const EmanoteApi_GetNotes) <$> askRoute
            resp <- App.requestingDynamic req
            mresp <- maybeDyn =<< holdDyn Nothing (Just <$> resp)
            dyn_ $
              ffor mresp $ \case
                Nothing -> loader
                Just v -> do
                  eresp <- eitherDyn v
                  dyn_ $
                    ffor eresp $ \case
                      Left err -> dynText err
                      Right notesDyn -> do
                        el "ul" $ do
                          void $
                            simpleList notesDyn $ \xDyn -> do
                              elClass "li" "mb-2" $ do
                                renderWikiLink mempty (constDyn WikiLinkLabel_Unlabelled) (snd <$> xDyn)
                                dyn_ $
                                  ffor (fst <$> xDyn) $ \case
                                    LinkStatus_Orphaned ->
                                      orphanLabel
                                    _ -> blank
        pure $ constDyn Nothing
      FrontendRoute_Note -> do
        req <- fmap EmanoteApi_Note <$> askRoute
        resp <- App.requestingDynamic req
        waiting <-
          holdDyn True $
            leftmost
              [ fmap (const True) (updated req),
                fmap (const False) resp
              ]
        elDynClass "div" (ffor waiting $ bool "" "animate-pulse") $ do
          mresp <- maybeDyn =<< holdDyn Nothing (Just <$> resp)
          dyn_ $
            ffor mresp $ \case
              Nothing -> do
                loader
              Just resp' -> do
                eresp <- eitherDyn resp'
                dyn_ $
                  ffor eresp $ \case
                    Left errDyn -> dynText $ show <$> errDyn
                    Right (noteDyn :: Dynamic t Note) -> do
                      divClass "grid gap-4 grid-cols-6" $ do
                        divClass "col-start-1 col-span-2" $ do
                          divClass "linksBox p-2" $ do
                            routeLink (FrontendRoute_Main :/ ()) $ text "Back to /"
                          divClass "linksBox animated" $ do
                            renderLinkContexts "Uplinks" (_note_uplinks <$> noteDyn) $ \ctx -> do
                              divClass "opacity-50 hover:opacity-100 text-sm" $ do
                                dyn_ $ renderPandoc <$> ctx
                          divClass "linksBox animated" $ do
                            renderLinkContexts "Backlinks" (_note_backlinks <$> noteDyn) $ \ctx -> do
                              divClass "opacity-50 hover:opacity-100 text-sm" $ do
                                dyn_ $ renderPandoc <$> ctx
                        divClass "col-start-3 col-span-4" $ do
                          el "h1" $ do
                            r <- askRoute
                            dynText $ untag <$> r
                          mzettel <- maybeDyn $ _note_zettel <$> noteDyn
                          dyn_ $
                            ffor mzettel $ \case
                              Nothing -> text "No such note"
                              Just zDyn -> do
                                ez <- eitherDyn zDyn
                                dyn_ $
                                  ffor ez $ \case
                                    Left conflict -> dynText $ show <$> conflict
                                    Right (fmap snd -> v) -> do
                                      edoc <- eitherDyn v
                                      dyn_ $
                                        ffor edoc $ \case
                                          Left parseErr -> dynText $ show <$> parseErr
                                          Right docDyn -> do
                                            dyn_ $ renderPandoc <$> docDyn
                          divClass "linksBox animated" $ do
                            renderLinkContexts "Downlinks" (_note_downlinks <$> noteDyn) $ \ctx -> do
                              divClass "opacity-50 hover:opacity-100 text-sm" $ do
                                dyn_ $ renderPandoc <$> ctx
                        divClass "col-start-1 col-span-6 place-self-center text-gray-400 border-t-2" $ do
                          text "Powered by "
                          elAttr "a" ("href" =: "https://github.com/srid/emanote") $
                            text "Emanote"
        fmap (Just . untag) <$> askRoute
  where
    renderLinkContexts name ls ctxW = do
      divClass name $ do
        elClass "h2" "header w-full pl-2 pt-2 pb-2 font-serif bg-green-100 " $ text name
        divClass "p-2" $ do
          void $
            simpleList ls $ \lDyn -> do
              divClass "pt-1" $ do
                divClass "linkheader" $
                  renderLinkContext ("class" =: "text-green-700") lDyn
                ctxW $ _linkcontext_ctx <$> lDyn
    renderLinkContext attrs lDyn = do
      renderWikiLink attrs (_linkcontext_label <$> lDyn) (_linkcontext_id <$> lDyn)
    renderWikiLink attrs lbl wId =
      routeLinkDynAttr
        ( ffor lbl $ \x ->
            "title" =: show x <> attrs
        )
        ( ffor wId $ \x ->
            FrontendRoute_Note :/ x
        )
        $ do
          dynText $ untag <$> wId
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

orphanLabel :: DomBuilder t m => m ()
orphanLabel = do
  elClass "span" "border-2 bg-red-600 text-white ml-2 text-sm rounded" $
    text "Orphaned"

loader :: DomBuilder t m => m ()
loader = do
  divClass "grid grid-cols-3 ml-0 pl-0 content-evenly" $ do
    divClass "col-start-1 col-span-3 h-16" blank
    divClass "col-start-2 col-span-1 place-self-center p-4 h-full bg-black text-white rounded" $
      text "Loading..."
