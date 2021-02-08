{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Backend where

import Common.Api
import Common.Route
import qualified Data.Aeson as Aeson
import Data.Constraint.Extras (has)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Some (Some (..))
import Data.Text as T
import qualified Emanote
import qualified Emanote.Graph as G
import qualified Emanote.Graph.Patch as GP
import Emanote.Markdown.WikiLink
import qualified Emanote.Markdown.WikiLink as W
import Emanote.Zk (Zk (..))
import qualified Emanote.Zk as Zk
import Network.WebSockets as WS
import Network.WebSockets.Snap as WS (runWebSocketsSnap)
import Obelisk.Backend (Backend (..))
import Obelisk.ExecutableConfig.Lookup (getConfigs)
import Obelisk.Route
import Reflex.Dom.GadtApi.WebSocket (mkTaggedResponse)
import Relude
import Snap.Core
import Text.Pandoc.Definition (Block, Pandoc (..))

backend :: Backend BackendRoute FrontendRoute
backend =
  Backend
    { _backend_run = \serve -> do
        configs <- getConfigs
        let getCfg k =
              maybe (error $ "Missing " <> k) (T.strip . decodeUtf8) $ Map.lookup k configs
            notesDir = toString $ getCfg "backend/notesDir"
        Emanote.emanoteMainWith notesDir $ \zk -> do
          serve $ \case
            BackendRoute_Missing :/ () -> do
              modifyResponse $ setResponseStatus 404 "Missing"
              writeText "Not found"
            BackendRoute_Api :/ () -> do
              mreq <- Aeson.decode <$> readRequestBody 16384
              case mreq of
                Nothing -> do
                  modifyResponse $ setResponseStatus 400 "Bad Request"
                  writeText "Bad response!"
                Just (Some emApi :: Some EmanoteApi) -> do
                  resp <- handleEmanoteApi zk emApi
                  writeLBS $ has @Aeson.ToJSON emApi $ Aeson.encode resp
            BackendRoute_WebSocket :/ () -> do
              runWebSocketsSnap $ \pc -> do
                conn <- WS.acceptRequest pc
                forever $ do
                  dm <- WS.receiveDataMessage conn
                  let m = Aeson.eitherDecode $ case dm of
                        WS.Text v _ -> v
                        WS.Binary v -> v
                  case m of
                    Right req -> do
                      -- TODO: Investigate the possibility of handleEmanoteApi
                      -- "streaming" changes for this `conn` (depending on its last request).
                      r <- mkTaggedResponse req $ handleEmanoteApi zk
                      case r of
                        Left err -> error $ toText err -- TODO
                        Right rsp ->
                          WS.sendDataMessage conn $
                            WS.Text (Aeson.encode rsp) Nothing
                    Left err -> error $ toText err --TODO
                  pure (),
      _backend_routeEncoder = fullRouteEncoder
    }

handleEmanoteApi :: MonadIO m => Zk -> EmanoteApi a -> m a
handleEmanoteApi zk@Zk {..} = \case
  EmanoteApi_GetRev -> do
    Zk.getRev zk
  EmanoteApi_GetNotes -> do
    liftIO $ putStrLn "GetNotes!"
    zs <- Zk.getZettels zk
    graph <- Zk.getGraph zk
    rev <- Zk.getRev zk
    let orphans =
          let getVertices = GP.patchedGraphVertexSet (isJust . flip Map.lookup zs) . G.unGraph
              indexed = getVertices $ G.filterBy (\l -> W.isBranch l || W.isParent l) graph
              all' = getVertices graph
           in all' `Set.difference` indexed
    pure $
      (rev,) $
        sort $
          Map.keys zs <&> \z ->
            bool (LinkStatus_Connected, z) (LinkStatus_Orphaned, z) $ Set.member z orphans
  EmanoteApi_Note wikiLinkID -> do
    liftIO $ putStrLn $ "Note! " <> show wikiLinkID
    zs <- Zk.getZettels zk
    graph <- Zk.getGraph zk
    rev <- Zk.getRev zk
    let mz = Map.lookup wikiLinkID zs
    let mkLinkCtxList f = do
          let ls = uncurry mkLinkContext <$> G.connectionsOf f wikiLinkID graph
          -- Sort in reverse order so that daily notes (calendar) are pushed down.
          sortOn Down ls
        wikiLinkUrl = renderWikiLinkUrl wikiLinkID
    let note =
          Note
            wikiLinkID
            wikiLinkUrl
            mz
            -- TODO: Refactor using enum/dmap/gadt
            (mkLinkCtxList (\l -> W.isReverse l && not (W.isParent l) && not (W.isBranch l))) -- Backlinks (sans uplinks / downlinks)
            (mkLinkCtxList W.isBranch) -- Downlinks
            (mkLinkCtxList W.isParent) -- Uplinks
    pure (rev, note)
  where
    mkLinkContext :: (WikiLinkLabel, [Block]) -> WikiLinkID -> LinkContext
    mkLinkContext (_linkcontext_label, Pandoc mempty -> _linkcontext_ctx) _linkcontext_id =
      LinkContext {..}
