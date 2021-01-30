{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module G.Markdown.WikiLink
  ( wikiLinkSpec,
    WikiLinkID,
    parseWikiLinkUrl,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Inlines as CM
import Commonmark.TokParsers (noneOfToks, symbol)
import Data.Tagged (Tagged (..), untag)
import qualified Data.Text as T
import qualified Text.Megaparsec as M
import qualified Text.Parsec as P

-- | The inner text of a wiki link.
type WikiLinkID = Tagged "WikiLinkID" Text

wikiLinkSpec ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il) =>
  CM.SyntaxSpec m il bl
wikiLinkSpec =
  mempty
    { CM.syntaxInlineParsers = [pLink]
    }
  where
    pLink ::
      (Monad m, CM.IsInline il) =>
      CM.InlineParser m il
    pLink =
      P.try $
        P.choice
          [ -- All neuron type links; not propagating link type for now.
            cmAutoLink <$> P.try (wikiLinkP 3),
            cmAutoLink <$> P.try (symbol '#' *> wikiLinkP 2),
            cmAutoLink <$> P.try (wikiLinkP 2 <* symbol '#'),
            cmAutoLink <$> P.try (wikiLinkP 2)
          ]
    wikiLinkP :: Monad m => Int -> P.ParsecT [CM.Tok] s m WikiLinkID
    wikiLinkP n = do
      void $ M.count n $ symbol '['
      s <-
        fmap CM.untokenize $
          some $
            noneOfToks [CM.Symbol ']', CM.Symbol '[', CM.LineEnd]
      void $ M.count n $ symbol ']'
      pure $ Tagged s
    cmAutoLink :: CM.IsInline a => WikiLinkID -> a
    cmAutoLink s =
      CM.link (renderWikiLinkUrl s) "" $ CM.str (untag s)

-- | Make [[Foo]] link to "Foo". In future, make this configurable.
renderWikiLinkUrl :: WikiLinkID -> Text
renderWikiLinkUrl (Tagged s) = s

-- | Parse what was rendered by renderWikiLinkUrl
parseWikiLinkUrl :: Text -> Maybe WikiLinkID
parseWikiLinkUrl s = do
  guard $ not $ ":" `T.isInfixOf` s
  guard $ not $ "/" `T.isInfixOf` s
  pure $ Tagged s