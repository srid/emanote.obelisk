{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Backend where

import Common.Api
import Common.Route
import Control.Concurrent.Async (race_)
import qualified Data.Aeson as Aeson
import Data.Constraint.Extras
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Some
import Data.Text as T
import qualified Emanote
import qualified Emanote.Graph as G
import qualified Emanote.Graph.Patch as GP
import Emanote.Markdown.WikiLink
import qualified Emanote.Markdown.WikiLink as W
import qualified Emanote.WebServer as WS
import Emanote.Zk (Zk (..))
import Network.WebSockets as WS
import Network.WebSockets.Snap as WS
import Obelisk.Backend
import Obelisk.ExecutableConfig.Lookup (getConfigs)
import Obelisk.Route
import Reflex.Dom.GadtApi.WebSocket
import qualified Reflex.TIncremental as TInc
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
          race_
            (WS.run notesDir zk)
            ( serve $ \case
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
                          r <- mkTaggedResponse req $ handleEmanoteApi zk
                          case r of
                            Left err -> error $ toText err -- TODO
                            Right rsp ->
                              WS.sendDataMessage conn $
                                WS.Text (Aeson.encode rsp) Nothing
                        Left err -> error $ toText err --TODO
                      pure ()
            ),
      _backend_routeEncoder = fullRouteEncoder
    }

handleEmanoteApi :: MonadIO m => Zk -> EmanoteApi a -> m a
handleEmanoteApi Zk {..} = \case
  EmanoteApi_GetNotes -> do
    liftIO $ putStrLn $ "GetNotes!"
    Map.keys <$> TInc.readValue _zk_zettels
  EmanoteApi_Note wikiLinkID -> do
    liftIO $ putStrLn $ "Note! " <> show wikiLinkID
    zs <- TInc.readValue _zk_zettels
    let mz = Map.lookup wikiLinkID zs
    graph <- TInc.readValue _zk_graph
    let mkLinkCtxList f = do
          let ls = uncurry mkLinkContext <$> G.connectionsOf f wikiLinkID graph
          -- Sort in reverse order so that daily notes (calendar) are pushed down.
          sortOn Down ls
        wikiLinkUrl = renderWikiLinkUrl wikiLinkID
        orphans =
          let getVertices = GP.patchedGraphVertexSet (isJust . flip Map.lookup zs) . G.unGraph
              indexed = getVertices $ G.filterBy (\l -> W.isBranch l || W.isParent l) graph
              all' = getVertices graph
              unindexed = all' `Set.difference` indexed
           in Set.toList unindexed <&> mkLinkContext (WikiLinkLabel_Unlabelled, mempty)
    let note =
          Note
            wikiLinkID
            wikiLinkUrl
            mz
            -- TODO: Refactor using enum/dmap/gadt
            (mkLinkCtxList (\l -> W.isReverse l && not (W.isParent l) && not (W.isBranch l))) -- Backlinks (sans uplinks / downlinks)
            (mkLinkCtxList W.isBranch) -- Downlinks
            (mkLinkCtxList W.isParent) -- Uplinks
            orphans
    pure note
  where
    mkLinkContext :: (WikiLinkLabel, [Block]) -> WikiLinkID -> LinkContext
    mkLinkContext (_linkcontext_label, Pandoc mempty -> _linkcontext_ctx) _linkcontext_id =
      let _linkcontext_url = renderWikiLinkUrl _linkcontext_id
       in LinkContext {..}
