{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend.Search where

import Common.Api
import Common.Route
import Control.Lens.Operators
import Control.Monad.Fix (MonadFix)
import qualified Data.Text as T
import Data.Time.Clock
import Emanote.Markdown.WikiLink
import Frontend.App
import qualified Frontend.Widget as W
import GHCJS.DOM.HTMLElement (focus)
import GHCJS.DOM.Types (IsHTMLElement)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Relude

-- TODO: Escape key to reset search box and remove focus
searchWidget ::
  forall t m js.
  ( DomBuilder t m,
    MonadHold t m,
    PostBuild t m,
    MonadFix m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m),
    RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    Prerender js t m,
    EmanoteRequester t m,
    IsHTMLElement (RawInputElement (DomBuilderSpace m))
  ) =>
  Event t () ->
  RoutedT t (R FrontendRoute) m ()
searchWidget trigger = do
  elFullPanel $ do
    clickedAway <- fmap updated askRoute
    let inputClass = "pl-2 my-0.5 w-full md:w-large rounded border border-transparent focus:outline-none focus:ring-2 focus:ring-green-600 focus:border-transparent"
    qElem <-
      inputElement $
        def
          & initialAttributes .~ ("placeholder" =: "Search ..." <> "class" =: inputClass)
          & inputElementConfig_setValue .~ ("" <$ clickedAway)
    prerender_ blank $
      widgetHold_ blank $
        ffor trigger $ \() ->
          focus $ _inputElement_raw qElem
    mq :: Dynamic t (Maybe Text) <- debounceDyn 0.2 $ ffor (value qElem) $ \(T.strip -> s) -> guard (not $ T.null s) >> pure s
    mq' <- maybeDyn mq
    dyn_ $
      ffor mq' $ \case
        Nothing -> blank
        Just q -> do
          let req = EmanoteApi_Search <$> q
          resp <- requestingDynamic req
          widgetHold_ blank $
            ffor (filterLeft resp) $ \err -> do
              divClass "bg-red-200" $ text err
          results <- holdDyn [] $ filterRight resp
          let linkAttrs = "class" =: "text-green-700"
          divClass "rounded bg-gray-100" $ do
            void $
              simpleList results $ \wId -> do
                divClass "p-0.5 pl-1" $ W.renderWikiLink linkAttrs (constDyn WikiLinkLabel_Unlabelled) wId

-- | Like @debounce@ but operates on a Dynamic instead
--
-- The initial value fires immediately, but the updated values will be debounced.
debounceDyn ::
  forall t m a.
  ( MonadFix m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m),
    MonadHold t m
  ) =>
  NominalDiffTime ->
  Dynamic t a ->
  m (Dynamic t a)
debounceDyn t x = do
  x0 <- sample $ current x
  let xE = updated x
  holdDyn x0 =<< debounce t xE

-- | Main column
-- TODO: move to Widget.hs?
elFullPanel :: (DomBuilder t m) => m a -> m a
elFullPanel =
  divClass "w-full overflow-hidden px-0.5 md:px-2 md:mt-2"
