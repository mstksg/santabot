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
import           Main.Master
import           Network.HTTP.Client.TLS
import           Network.HTTP.Conduit
import           Numeric.Natural
import           Santabot.Bot
import           Santabot.Run.Slack
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
    { cTick        :: Natural            -- ^ in seconds
    , cToken       :: T.Text             -- ^ API token
    , cPort        :: Natural
    , cBotConf     :: BotConf
    }
  deriving Generic

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
    mgr <- newTlsManager
    intcodeMap <- newIORef mempty
    launchSlack
      mgr
      (fromIntegral cPort)
      (fromIntegral cTick * 1000000)
      (masterBot cBotConf mgr intcodeMap phrasebook)
