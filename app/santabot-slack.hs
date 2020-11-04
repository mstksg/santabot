{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

import           Advent
import           Advent.Module.Intcode
import           Control.Monad.Reader
import           Data.Conduit.Lift
import           Data.Foldable
import           Data.IORef
import           Data.Map                              (Map)
import           Data.Time.Format
import           GHC.Generics
import           Main.Bot
import           Network.HTTP.Client.TLS
import           Network.HTTP.Conduit
import           Numeric.Natural
import           Santabot.Bot
import           Santabot.Run.IRC
import qualified Data.Aeson                            as A
import qualified Data.Set                              as S
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import qualified Data.Text.IO                          as T
import qualified Data.Text.Prettyprint.Doc             as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP
import qualified Data.Yaml                             as Y
import qualified Dhall                                 as D
import qualified Dhall.Pretty                          as D
import qualified Language.Haskell.Printf               as P
import qualified Text.Casing                           as Case

data Conf = Conf
    { cTick        :: Natural          -- ^ in seconds
    , cToken       :: T.Text             -- ^ API token
    , cBotConf     :: BotConf
    }
  deriving Generic

instance A.FromJSON Conf where
    parseJSON = A.genericParseJSON A.defaultOptions
      { A.fieldLabelModifier = A.camelTo2 '-' . drop 1
      }

instance A.ToJSON Conf where
    toJSON = A.genericToJSON A.defaultOptions
      { A.fieldLabelModifier = A.camelTo2 '-' . drop 1
      }

instance D.FromDhall Conf where
    autoWith _ = D.genericAutoWith $ D.defaultInterpretOptions
      { D.fieldModifier = T.pack . Case.camel . drop 1 . T.unpack }

instance D.ToDhall Conf where
    injectWith _ = D.genericToDhallWith $ D.defaultInterpretOptions
      { D.fieldModifier = T.pack . Case.camel . drop 1 . T.unpack }

main :: IO ()
main = do
    c@Conf{..} <- D.inputFile D.auto "santabot-conf-slack.dhall"
    PP.putDoc $ D.prettyExpr (D.embed D.inject c)
    putStrLn ""
    phrasebook <- S.fromList . map T.pack . lines <$> readFile "phrasebook.txt"
    print phrasebook
    -- c@Conf{..} <- Y.decodeFileThrow "santabot-slack-conf.yaml"
    -- T.putStrLn . T.decodeUtf8 . Y.encode $ c
    -- phrasebook <- S.fromList . map T.pack . lines <$> readFile "phrasebook.txt"
    -- mgr <- newTlsManager
    -- intcodeMap <- newIORef mempty
    -- launchIRC (toList (bcChannels cBotConf)) cNick cPassword (cTick * 1000000)
    --     (masterBot cBotConf mgr intcodeMap phrasebook)
