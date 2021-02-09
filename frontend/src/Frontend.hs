{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Common.Api
import Common.Route
import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import Data.Tagged
import qualified Data.Text as T
import Emanote.Markdown.WikiLink
import qualified Emanote.Zk.Type as Zk
import qualified Frontend.App as App
import qualified Frontend.Static as Static
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import qualified Reflex.Dom.Pandoc as PR
import Relude
import Text.Pandoc.Definition (Pandoc)

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
          dynText $ fromMaybe "..." <$> titDyn
          text " | Emanote"
        Static.includeAssets,
      _frontend_body = do
        divClass "min-h-screen md:container mx-auto px-4" $ do
          fmap join $
            prerender (pure $ constDyn Nothing) $ do
              App.runApp $ do
                rec xDyn <- app $ traceEvent "updateAvailable" update
                    let rev = fmapMaybe id $ updated $ fst <$> xDyn
                    update <- App.pollRevUpdates EmanoteApi_GetRev rightToMaybe rev
                pure $ snd <$> xDyn
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
  Event t Zk.Rev ->
  RoutedT t (R FrontendRoute) m (Dynamic t (Maybe Zk.Rev, Maybe Text))
app updateAvailable = do
  divClass "flex flex-wrap justify-center flex-row-reverse md:-mx-2 overflow-hidden" $ do
    fmap join $
      subRoute $ \case
        FrontendRoute_Main -> do
          divClass "w-full md:w-1/2 overflow-hidden md:my-2 md:px-2" $ do
            el "h1" $ text "Emanote"
            elClass "p" "rounded border-2 mt-2 mb-2 p-2" $
              text "Welcome to Emanote. This place will soon look like a search engine, allowing you to query your notebook graph. For now, we simply display the list of notes."
            req <- fmap (const EmanoteApi_GetNotes) <$> askRoute
            resp <- App.requestingDynamicWithRefreshEvent req (() <$ updateAvailable)
            mresp <- maybeDyn =<< holdDyn Nothing (Just <$> resp)
            (fmap join . holdDyn (constDyn (Nothing, Nothing))) <=< dyn $
              ffor mresp $ \case
                Nothing -> do
                  loader
                  pure $ constDyn (Nothing, Nothing)
                Just v -> do
                  eresp <- eitherDyn v
                  (fmap join . holdDyn (constDyn (Nothing, Nothing))) <=< dyn $
                    ffor eresp $ \case
                      Left err -> do
                        dynText err
                        pure $ constDyn (Nothing, Just "Err")
                      Right result -> do
                        let notesDyn = snd <$> result
                            revDyn = fst <$> result
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
                        pure $ ffor revDyn $ \rev -> (Just rev, Just "Home")
        FrontendRoute_Note -> do
          req <- fmap EmanoteApi_Note <$> askRoute
          resp <- App.requestingDynamicWithRefreshEvent req (() <$ updateAvailable)
          waiting <-
            holdDyn True $
              leftmost
                [ fmap (const True) (updated req),
                  fmap (const False) resp
                ]
          currentRev :: Dynamic t (Maybe Zk.Rev) <- noteWidget waiting resp
          titleDyn <- fmap (Just . untag) <$> askRoute
          pure $ (,) <$> currentRev <*> titleDyn

noteWidget ::
  forall js t m.
  ( DomBuilder t m,
    MonadHold t m,
    PostBuild t m,
    RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    Prerender js t m,
    MonadFix m
  ) =>
  Dynamic t Bool ->
  Event t (Either Text (Zk.Rev, Note)) ->
  RoutedT t WikiLinkID m (Dynamic t (Maybe Zk.Rev))
noteWidget waiting resp = do
  let divClassMayLoading cls =
        elDynClass "div" (ffor waiting $ bool cls (cls <> " animate-pulse"))
  mresp <- maybeDyn =<< holdDyn Nothing (Just <$> resp)
  (fmap join . holdDyn (constDyn Nothing)) <=< dyn $
    ffor mresp $ \case
      Nothing -> do
        loader
        pure $ constDyn Nothing
      Just resp' -> do
        eresp <- eitherDyn resp'
        (fmap join . holdDyn (constDyn Nothing)) <=< dyn $
          ffor eresp $ \case
            Left errDyn -> do
              dynText $ show <$> errDyn
              pure $ constDyn Nothing
            Right result -> do
              let noteDyn = snd <$> result
                  revDyn :: Dynamic t Zk.Rev = fst <$> result
                  uplinks = _note_uplinks <$> noteDyn
                  backlinks = _note_backlinks <$> noteDyn
                  downlinks = _note_downlinks <$> noteDyn
              do
                divClassMayLoading "w-full overflow-hidden md:my-2 md:px-2 md:w-4/6" $ do
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
                  renderLinkContexts "Downlinks" downlinks $ \ctx -> do
                    divClass "opacity-50 hover:opacity-100 text-sm" $ do
                      dyn_ $ renderPandoc <$> ctx
                divClassMayLoading "w-full overflow-hidden md:my-2 md:px-2 md:w-2/6" $ do
                  divClass "" $ do
                    routeLink (FrontendRoute_Main :/ ()) $
                      elClass "button" "font-serif border-1 p-2 text-white rounded place-self-center border-green-700 bg-green-400 hover:opacity-70" $
                        text "Home"
                  renderLinkContexts "Uplinks" uplinks $ \ctx -> do
                    divClass "opacity-50 hover:opacity-100 text-sm" $ do
                      dyn_ $ renderPandoc <$> ctx
                  renderLinkContexts "Backlinks" backlinks $ \ctx -> do
                    divClass "opacity-50 hover:opacity-100 text-sm" $ do
                      dyn_ $ renderPandoc <$> ctx
                divClass "w-full md:my-2 md:px-2 content-center text-gray-400 border-t-2" $ do
                  text "Powered by "
                  elAttr "a" ("href" =: "https://github.com/srid/emanote") $
                    text "Emanote"
                  text " ("
                  el "tt" $ dynText $ show . untag <$> revDyn
                  text " changes since boot)"
              pure $ Just <$> revDyn
  where
    renderLinkContexts name ls ctxW = do
      let mkDivClass hide =
            T.intercalate " " $
              catMaybes
                [ bool Nothing (Just "hidden") hide,
                  Just "linksBox",
                  Just "animated",
                  Just name
                ]
      elDynClass "div" (mkDivClass . null <$> ls) $ do
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
    $ do
      dynText $ untag <$> wId

renderPandoc ::
  ( PostBuild t m,
    RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    Prerender js t m,
    DomBuilder t m
  ) =>
  Pandoc ->
  m ()
renderPandoc doc = do
  let cfg =
        PR.defaultConfig
          { PR._config_renderLink = linkRender
          }
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
