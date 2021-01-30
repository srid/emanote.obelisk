module G.WebServer (run) where

import qualified Data.Map.Strict as Map
import Data.Tagged (Tagged (Tagged))
import G.Db (Db (..))
import G.Db.Types.Zk (Zk (..))
import Reflex.Dom.Core
import qualified Reflex.Dom.Pandoc as PR
import qualified Shower
import Web.Scotty (html, param, scotty)
import qualified Web.Scotty as Scotty

run ::
  Db ->
  IO ()
run Db {..} = do
  scotty 3000 $ do
    Scotty.get "/:wikiLinkID" $ do
      wikiLinkID <- Tagged <$> param "wikiLinkID"
      Zk {..} <- liftIO $ readTVarIO _db_data
      case Map.lookup wikiLinkID _zk_zettels of
        Nothing -> Scotty.text "404"
        Just v ->
          case v of
            Left err -> Scotty.text $ "Conflic: " <> show err
            Right (_fp, Left err) -> Scotty.text $ "Parse: " <> show err
            Right (_fp, Right doc) -> do
              s <- fmap snd $
                liftIO $
                  renderStatic $ do
                    PR.elPandoc PR.defaultConfig doc
                    el "pre" $ text $ toText $ Shower.shower _zk_graph
              html $ decodeUtf8 s
