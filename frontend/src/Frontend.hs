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
import Reflex.Dom.Core hiding (Link)
import qualified Reflex.Dom.Pandoc as PR
import Relude
import Skylighting.Format.HTML (styleToCss)
import qualified Skylighting.Styles as SkylightingStyles
import Text.Pandoc.Definition (Pandoc (..))

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
        elAttr "style" ("type" =: "text/css") $ text $ toText $ styleToCss SkylightingStyles.espresso
        Static.includeAssets,
      _frontend_body = do
        divClass "min-h-screen md:container mx-auto px-4" $ do
          fmap join $
            prerender (pure $ constDyn Nothing) $ do
              App.runApp $ do
                rec xDyn <- app update
                    let rev = fmapMaybe (nonReadOnlyRev =<<) $ updated $ fst <$> xDyn
                    update <- App.pollRevUpdates EmanoteApi_GetRev rightToMaybe rev
                pure $ snd <$> xDyn
    }
  where
    nonReadOnlyRev = \case
      EmanoteState_AtRev rev -> Just rev
      _ -> Nothing

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
  RoutedT t (R FrontendRoute) m (Dynamic t (Maybe EmanoteState, Maybe Text))
app updateAvailable = do
  divClass "flex flex-wrap justify-center flex-row-reverse md:-mx-2 overflow-hidden" $ do
    fmap join $
      subRoute $ \case
        FrontendRoute_Main -> do
          req <- fmap (const EmanoteApi_GetNotes) <$> askRoute
          resp <- App.requestingDynamicWithRefreshEvent req (() <$ updateAvailable)
          waiting <-
            holdDyn True $
              leftmost
                [ fmap (const True) (updated req),
                  fmap (const False) resp
                ]
          fmap (,Just "Home") <$> homeWidget waiting resp
        FrontendRoute_Note -> do
          req <- fmap EmanoteApi_Note <$> askRoute
          resp <- App.requestingDynamicWithRefreshEvent req (() <$ updateAvailable)
          waiting <-
            holdDyn True $
              leftmost
                [ fmap (const True) (updated req),
                  fmap (const False) resp
                ]
          currentRev :: Dynamic t (Maybe EmanoteState) <- noteWidget waiting resp
          titleDyn <- fmap (Just . untag) <$> askRoute
          pure $ (,) <$> currentRev <*> titleDyn

homeWidget ::
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
  Event t (Either Text (EmanoteState, [(Affinity, WikiLinkID)])) ->
  RoutedT t () m (Dynamic t (Maybe EmanoteState))
homeWidget waiting resp = do
  elMainPanel waiting $ do
    elMainHeading $ text "Emanote"
    elClass "p" "rounded border-2 mt-2 mb-2 p-2" $
      text "Welcome to Emanote. This place will soon look like a search engine, allowing you to query your notebook graph. For now, we simply display root folgezettels and orphans (if any)."
    withBackendResponse resp (constDyn Nothing) $ \result -> do
      let notesDyn = snd <$> result
          stateDyn = fst <$> result
      el "ul" $ do
        void $
          simpleList notesDyn $ \xDyn -> do
            elClass "li" "mb-2" $ do
              renderWikiLink mempty (constDyn WikiLinkLabel_Unlabelled) (snd <$> xDyn)
              dyn_ $
                affinityLabel . fst <$> xDyn
      pure $ Just <$> stateDyn

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
  Event t (Either Text (EmanoteState, Note)) ->
  RoutedT t WikiLinkID m (Dynamic t (Maybe EmanoteState))
noteWidget waiting resp = do
  withBackendResponse resp (constDyn Nothing) $ \result -> do
    let noteDyn = snd <$> result
        stateDyn :: Dynamic t EmanoteState = fst <$> result
        uplinks = _note_uplinks <$> noteDyn
        backlinks = _note_backlinks <$> noteDyn
        downlinks = _note_downlinks <$> noteDyn
    elMainPanel waiting $ do
      elMainHeading $ do
        r <- askRoute
        dynText $ untag <$> r
      mzettel <- maybeDyn $ _note_zettel <$> noteDyn
      dyn_ $
        ffor mzettel $ \case
          Nothing -> do
            -- We allow non-existant notes, if they have backlinks, etc.
            hasRefs <- holdUniqDyn $
              ffor noteDyn $ \Note {..} ->
                not $ null _note_uplinks && null _note_backlinks && null _note_downlinks
            dyn_ $
              ffor hasRefs $ \case
                True -> blank
                False -> text "No such note"
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
                        divClass "notePandoc" $
                          dyn_ $ renderPandoc <$> docDyn
      renderLinkContexts "Tagged by" downlinks $ \ctx -> do
        divClass "opacity-50 hover:opacity-100 text-sm" $ do
          renderLinkContextBody ctx
    elSidePanel waiting $ do
      divClass "" $ do
        routeLink (FrontendRoute_Main :/ ()) $
          elClass "button" "font-serif border-1 p-2 text-white rounded place-self-center border-green-700 bg-green-400 hover:opacity-70" $
            text "Home"
      renderLinkContexts "Uplinks" uplinks $ \ctx -> do
        divClass "opacity-50 hover:opacity-100 text-sm" $ do
          renderLinkContextBody ctx
      renderLinkContexts "Backlinks" backlinks $ \ctx -> do
        divClass "opacity-50 hover:opacity-100 text-sm" $ do
          renderLinkContextBody ctx
    elFooter $ do
      let url = "https://github.com/srid/emanote"
      text "Powered by "
      elAttr "a" ("href" =: url) $
        text "Emanote"
      dyn_ $
        ffor stateDyn $ \case
          EmanoteState_ReadOnly -> blank
          EmanoteState_AtRev rev -> do
            text " ("
            el "tt" $ text $ show $ untag rev
            text " changes since boot)"
    pure $ Just <$> stateDyn
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
        elClass "h2" "header text-xl w-full pl-2 pt-2 pb-2 font-serif bg-green-100 " $ text name
        divClass "p-2" $ do
          void $
            simpleList ls $ \lDyn -> do
              divClass "pt-1" $ do
                divClass "linkheader" $
                  renderLinkContextLink ("class" =: "text-green-700") lDyn
                ctxW $ _linkcontext_ctxList <$> lDyn
    renderLinkContextLink attrs lDyn = do
      renderWikiLink
        attrs
        (_linkcontext_effectiveLabel <$> lDyn)
        (_linkcontext_id <$> lDyn)
    renderLinkContextBody (ctxs :: Dynamic t [WikiLinkContext]) = do
      void $
        simpleList ctxs $ \ctx -> do
          divClass "mb-1 pb-1 border-b-2 border-black-200" $
            dyn_ $ renderPandoc . Pandoc mempty <$> ctx

divClassMayLoading :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> Text -> m a -> m a
divClassMayLoading waiting cls =
  elDynClass "div" (ffor waiting $ bool cls (cls <> " animate-pulse"))

elMainPanel :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> m a -> m a
elMainPanel waiting =
  divClassMayLoading waiting "w-full overflow-hidden md:my-2 md:px-2 md:w-4/6"

elMainHeading :: DomBuilder t m => m a -> m a
elMainHeading =
  elClass "h1" "text-3xl text-green-700 font-bold mt-2 mb-4"

elSidePanel :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> m a -> m a
elSidePanel waiting =
  divClassMayLoading waiting "w-full overflow-hidden md:my-2 md:px-2 md:w-2/6"

elFooter :: (DomBuilder t m) => m a -> m a
elFooter =
  divClass "w-full md:my-2 md:px-2 content-center text-gray-400 border-t-2"

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

affinityLabel :: DomBuilder t m => Affinity -> m ()
affinityLabel = \case
  Affinity_Orphaned ->
    elClass "span" "border-2 bg-red-600 text-white ml-2 p-0.5 text-sm rounded" $
      text "Orphaned"
  Affinity_Root ->
    elClass "span" "border-2 bg-purple-600 text-white ml-2 p-0.5 text-sm rounded" $
      text "Root"
  Affinity_HasParents n ->
    elClass "span" "border-2 text-gray ml-2 p-0.5 text-sm rounded" $ do
      elAttr "span" ("title" =: (show n <> " parents")) $
        text $ show n

loader :: DomBuilder t m => m ()
loader = do
  divClass "grid grid-cols-3 ml-0 pl-0 content-evenly" $ do
    divClass "col-start-1 col-span-3 h-16" blank
    divClass "col-start-2 col-span-1 place-self-center p-4 h-full bg-black text-white rounded" $
      text "Loading..."

-- Handle a response event from backend, and invoke the given widget for the
-- actual result.
--
-- This function does loading state and error handling.
withBackendResponse ::
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  -- | Response event from backend
  Event t (Either Text result) ->
  -- | The value to return when the result is not yet available or successful.
  Dynamic t v ->
  -- | Widget to render when the successful result becomes available
  (Dynamic t result -> m (Dynamic t v)) ->
  m (Dynamic t v)
withBackendResponse resp v0 f = do
  mresp <- maybeDyn =<< holdDyn Nothing (Just <$> resp)
  (fmap join . holdDyn v0) <=< dyn $
    ffor mresp $ \case
      Nothing -> do
        loader
        pure v0
      Just resp' -> do
        eresp <- eitherDyn resp'
        (fmap join . holdDyn v0) <=< dyn $
          ffor eresp $ \case
            Left errDyn -> do
              dynText $ show <$> errDyn
              pure v0
            Right result ->
              f result
