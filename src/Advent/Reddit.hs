{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Advent.Reddit (
  getPostLinks,
  cachedPostLinks,
  getPostLink,
  checkUncapped,
) where

import Advent
import Advent.Cache
import Control.Monad
import Control.Monad.Combinators
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Either
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Void
import qualified Data.Yaml as Y
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Directory
import Text.HTML.TagSoup.Tree (TagTree (..))
import qualified Text.HTML.TagSoup.Tree as T
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as P
import URI.ByteString

checkUncapped :: String -> IO Bool
checkUncapped u = fmap isJust . runMaybeT $ do
  Just req <- pure $ parseRequest u
  mgr <- liftIO newTlsManager
  Right t <- T.decodeUtf8' . BSL.toStrict . responseBody <$> liftIO (httpLbs req mgr)
  guard $ "capped" `T.isInfixOf` T.map toLower t

getPostLink :: Integer -> Day -> IO (Maybe String)
getPostLink y d = runMaybeT $ do
  guard =<< liftIO (challengeReleased y d)
  mp <- liftIO cachedPostLinks
  case M.lookup d =<< M.lookup y mp of
    Nothing -> do
      mp' <- liftIO $ do
        removeFile cachePath
        cachedPostLinks
      maybe empty pure $
        M.lookup d =<< M.lookup y mp'
    Just u -> pure u

cachedPostLinks :: IO (Map Integer (Map Day String))
cachedPostLinks =
  cacheing cachePath sl $
    getPostLinks =<< newTlsManager
  where
    sl =
      SL
        { _slSave = Just . T.decodeUtf8 . Y.encode
        , _slLoad =
            either (const Nothing) Just
              . Y.decodeEither'
              . T.encodeUtf8
        }

cachePath :: FilePath
cachePath = "cache/postlinks.yaml"

getPostLinks :: Manager -> IO (Map Integer (Map Day String))
getPostLinks =
  fmap
    ( (fmap . fmap) (T.unpack . T.decodeUtf8 . serializeURIRef')
        . parsePostLinks
        . T.decodeUtf8
        . BSL.toStrict
        . responseBody
    )
    . httpLbs wikiList

wikiList :: Request
wikiList =
  fromMaybe err $
    parseRequest
      "https://www.reddit.com/r/adventofcode/wiki/solution_megathreads?show_source"
  where
    err = error "Solutions megathread request not parsed"

parsePostLinks :: Text -> Map Integer (Map Day URI)
parsePostLinks =
  M.unionsWith (<>)
    . map (fromRight M.empty . parse parseLinks "solution_megathreads")
    . mapMaybe findTheDiv
    . T.universeTree
    . T.parseTree
  where
    findTheDiv (TagBranch "div" _ cld) = r <$ guard ("Solution Megathreads" `T.isInfixOf` r)
      where
        r = T.renderTree cld
    findTheDiv _ = Nothing

type Parser = Parsec Void Text

data Tok
  = TokYear Integer
  | TokLink Day URI

lexeme :: Parser a -> Parser a
lexeme = try . fmap snd . manyTill_ (try anySingle)

anyTok :: Parser Tok
anyTok = lexeme (asum [TokYear <$> try newYear, uncurry TokLink <$> try dayLink])

newYear :: Parser Integer
newYear = try $ "## December " *> P.decimal

dayLink :: Parser (Day, URI)
dayLink = do
  DayInt d <- "[" *> P.decimal <* "]"
  "("
  _ <- optional "https://redd.it"
  "/"
  l <- some (satisfy isAlphaNum)
  void ")"
  let pUri =
        parseURI strictURIParserOptions . T.encodeUtf8 . T.pack $
          "https://redd.it/" ++ l
  case pUri of
    Left x -> fail $ show x
    Right y -> pure (d, y)

parseLinks :: Parser (Map Integer (Map Day URI))
parseLinks = M.fromList <$> many (try parseYear)
  where
    parseYear :: Parser (Integer, Map Day URI)
    parseYear = do
      TokYear y <- anyTok
      (y,) . M.fromList <$> many (try parseDay)
    parseDay :: Parser (Day, URI)
    parseDay = do
      TokLink d l <- anyTok
      pure (d, l)
