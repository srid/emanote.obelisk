{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module G.WebServer (run) where

import qualified Data.Map.Strict as Map
import Data.Tagged (Tagged (Tagged))
import G.Db (Db (..), Zk (..))
import Reflex.Dom.Builder.Static (renderStatic)
import qualified Reflex.Dom.Pandoc as PR
import qualified Shower
import Text.Pandoc.Definition (Pandoc)
import Web.Scotty (html, param, scotty, text)
import qualified Web.Scotty as Scotty

run ::
  FilePath ->
  Db ->
  IO ()
run _outputDir Db {..} = do
  scotty 3000 $ do
    Scotty.get "/:wikiLinkID" $ do
      wikiLinkID <- Tagged <$> param "wikiLinkID"
      g <- _zk_graph <$> liftIO (readTVarIO _db_data)
      zs <- _zk_zettels <$> liftIO (readTVarIO _db_data)
      case Map.lookup wikiLinkID zs of
        Nothing -> text "404"
        Just v ->
          case v of
            Left err -> text $ "Conflic: " <> show err
            Right (_fp, Left err) -> text $ "Parse: " <> show err
            Right (_fp, Right doc) -> do
              s <- liftIO (renderPandoc doc)
              html $ decodeUtf8 s <> ("<pre>" <> toLText (Shower.shower g) <> "</pre>")
  where
    renderPandoc :: Pandoc -> IO ByteString
    renderPandoc =
      fmap snd . renderStatic . PR.elPandoc PR.defaultConfig
