{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Backend where

import qualified Algebra.Graph.Labelled.AdjacencyMap.Patch as GP
import Common.Api
import Common.Route
import qualified Common.Search as Search
import qualified Data.Aeson as Aeson
import Data.Constraint.Extras (has)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Some (Some (..))
import Data.Tagged
import qualified Data.Text as T
import qualified Emanote
import qualified Emanote.Graph as G
import qualified Emanote.Markdown as M
import Emanote.Markdown.WikiLink
import qualified Emanote.Markdown.WikiLink as W
import qualified Emanote.Markdown.WikiLink.Parser as M
import qualified Emanote.Pipeline as Pipeline
import Emanote.Zk (Zk (..))
import qualified Emanote.Zk as Zk
import GHC.Natural (intToNatural)
import Network.WebSockets as WS
import Network.WebSockets.Snap as WS (runWebSocketsSnap)
import Obelisk.Backend (Backend (..))
import Obelisk.ExecutableConfig.Lookup (getConfigs)
import Obelisk.Route
import Reflex.Dom.GadtApi.WebSocket (mkTaggedResponse)
import Relude
import Snap.Core
import System.TimeIt (timeItNamed)
import Text.Pandoc.Definition (Pandoc)

backend :: Backend BackendRoute FrontendRoute
backend =
  Backend
    { _backend_run = \serve -> do
        configs <- getConfigs
        let getCfg k =
              maybe (error $ "Missing " <> k) (T.strip . decodeUtf8) $ Map.lookup k configs
            notesDir = toString $ getCfg "backend/notesDir"
            readOnly = fromMaybe (error "Bad Bool value") $ readMaybe @Bool . toString $ getCfg "backend/readOnly"
            siteBlurb =
              either (error . show) id $
                M.parseMarkdown (M.wikiLinkSpec <> M.markdownSpec) "backend/siteBlurb" $ getCfg "backend/siteBlurb.md"
        Emanote.emanoteMainWith (bool Pipeline.run Pipeline.runNoMonitor readOnly notesDir) $ \zk -> do
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
                  resp <- handleEmanoteApi readOnly siteBlurb zk emApi
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
                      r <- mkTaggedResponse req $ handleEmanoteApi readOnly siteBlurb zk
                      case r of
                        Left err -> error $ toText err
                        Right rsp ->
                          WS.sendDataMessage conn $
                            WS.Text (Aeson.encode rsp) Nothing
                    Left err -> error $ toText err
                  pure (),
      _backend_routeEncoder = fullRouteEncoder
    }

handleEmanoteApi :: forall m a. MonadIO m => Bool -> Pandoc -> Zk -> EmanoteApi a -> m a
handleEmanoteApi readOnly siteBlurb zk@Zk {..} = \case
  EmanoteApi_GetRev -> do
    Zk.getRev zk
  EmanoteApi_Search (Search.parseSearchQuery -> q) -> do
    timeItNamed ("API: Search " <> show q) $ do
      graph <- Zk.getGraph zk
      zs <- Zk.getZettels zk
      let getVertices = GP.patchedGraphVertexSet (`Map.member` zs) . G.unGraph
          wIds = getVertices graph
          results = search q (toList wIds)
      pure $ take 10 results
  EmanoteApi_GetNotes -> do
    timeItNamed "API: GetNotes" $ do
      estate <- getEmanoteState
      toplevel <- getOrphansAndRoots
      pure (estate, (siteBlurb, sortOn Down toplevel))
  EmanoteApi_Note wikiLinkID -> do
    timeItNamed ("API: Note " <> show wikiLinkID) $ do
      estate <- getEmanoteState
      zs <- Zk.getZettels zk
      graph <- Zk.getGraph zk
      let mz = Map.lookup wikiLinkID zs
          mkLinkCtxList :: forall k. Ord k => (Directed WikiLinkLabel -> Bool) -> (LinkContext -> Down k) -> [LinkContext]
          mkLinkCtxList f sortField = do
            let conns =
                  Map.toList $
                    Map.fromListWith (<>) $
                      second one . swap
                        <$> G.connectionsOf f wikiLinkID graph
                ls =
                  conns <&> \(_linkcontext_id, wls :: NonEmpty WikiLink) ->
                    let _linkcontext_effectiveLabel = sconcat $ _wikilink_label <$> wls
                        _linkcontext_ctxList = mapMaybe _wikilink_ctx $ toList wls
                     in LinkContext {..}
            sortOn sortField ls
          note =
            Note
              wikiLinkID
              mz
              -- Sort backlinks by ID, so as to effectively push daily notes to
              -- the bottom.
              (mkLinkCtxList W.isBacklink $ Down . _linkcontext_id)
              -- Downlinks are sorted by context, so as to allow the user to
              -- control their sort order.
              --
              -- This useful for Blog downlinks, which typically link to date in
              -- their context, thus effecting allowing a listing that is sorted
              -- by date (mimicking blog timeline)
              (mkLinkCtxList W.isTaggedBy $ Down . _linkcontext_ctxList)
              -- See note above re: backlinks.
              (mkLinkCtxList W.isUplink $ Down . _linkcontext_id)
      pure (estate, note)
  where
    getEmanoteState :: m EmanoteState
    getEmanoteState = do
      if readOnly
        then pure EmanoteState_ReadOnly
        else EmanoteState_AtRev <$> Zk.getRev zk
    getOrphansAndRoots :: m [(Affinity, WikiLinkID)]
    getOrphansAndRoots = do
      zs <- Zk.getZettels zk
      graph <- Zk.getGraph zk
      let getVertices = GP.patchedGraphVertexSet (`Map.member` zs) . G.unGraph
          orphans =
            let indexed = getVertices $ G.filterBy (\l -> W.isBranch l || W.isParent l) graph
             in getVertices graph `Set.difference` indexed
          getAffinity = \case
            z | Set.member z orphans -> Affinity_Orphaned
            z -> case length (G.connectionsOf W.isParent z graph) of
              0 -> Affinity_Root
              n -> Affinity_HasParents (intToNatural n)
          topLevelNotes =
            flip mapMaybe (Set.toList $ getVertices graph) $ \z ->
              case getAffinity z of
                Affinity_HasParents _ -> Nothing
                aff -> Just (aff, z)
      pure topLevelNotes

search :: Search.SearchQuery -> [WikiLinkID] -> [WikiLinkID]
search =
  filter . match
  where
    match :: Search.SearchQuery -> WikiLinkID -> Bool
    match q wId =
      case q of
        Search.SearchQuery_TitleContains titleQ ->
          T.toLower titleQ `T.isInfixOf` T.toLower (untag wId)
        Search.SearchQuery_All ->
          True
        Search.SearchQuery_And subQueries ->
          all (`match` wId) subQueries
        Search.SearchQuery_Or subQueries ->
          any (`match` wId) subQueries
        Search.SearchQuery_BranchesFrom _ ->
          error "not implemented"
