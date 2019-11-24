{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Advent.Reddit (
    getPostLinks
  ) where

import           Advent
import           Control.Monad
import           Control.Monad.Combinators
import           Data.Char
import           Data.Foldable
import           Data.Map                   (Map)
import           Data.Maybe
import           Data.Text                  (Text)
import           Data.Void
import           Network.HTTP.Client
import           Text.HTML.TagSoup.Tree     (TagTree(..))
import           Text.Megaparsec
import           URI.ByteString
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Text.HTML.TagSoup.Tree     as T
import qualified Text.Megaparsec.Char.Lexer as P

getPostLinks :: Manager -> IO (Map Integer (Map Day URI))
getPostLinks = fmap ( parsePostLinks
                    . T.decodeUtf8
                    . BSL.toStrict
                    . responseBody
                    )
             . httpLbs wikiList


wikiList :: Request
Just wikiList = parseRequest
  "https://www.reddit.com/r/adventofcode/wiki/solution_megathreads?show_source"

parsePostLinks :: Text -> Map Integer (Map Day URI)
parsePostLinks = M.unionsWith (<>)
               . map (either (const M.empty) id . parse parseLinks "solution_megathreads")
               . mapMaybe findTheDiv
               . T.universeTree
               . T.parseTree
  where
    findTheDiv (TagBranch "div" _ cld) = r <$ guard ("Solution Megathreads" `T.isInfixOf` r)
      where
        r = T.renderTree cld
    findTheDiv _ = Nothing

type Parser = Parsec Void Text

data Tok = TokYear Integer
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
    ")"
    Right l' <- pure $
      parseURI strictURIParserOptions . T.encodeUtf8 . T.pack $
        "https://redd.it/" ++ l
    pure (d, l')

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
