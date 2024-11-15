{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Data.IORef
import qualified Data.Text as T
import qualified Dhall as D
import qualified Dhall.Pretty as D
import GHC.Generics
import Main.Master
import Network.HTTP.Client.TLS
import Numeric.Natural
import qualified Options.Applicative as O
import qualified Prettyprinter.Render.Text as PP
import Santabot.Run.IRC
import qualified Text.Casing as Case

data Conf = Conf
  { cTick :: Natural
  -- ^ in seconds
  , cNick :: String
  -- ^ bot username
  , cPassword :: Maybe String
  , cSasl :: Bool
  , cChannels :: [String]
  , cBotConf :: BotConf
  , cServer :: String
  }
  deriving (Generic)

instance D.FromDhall Conf where
  autoWith _ =
    D.genericAutoWith $
      D.defaultInterpretOptions
        { D.fieldModifier = T.pack . Case.camel . drop 1 . T.unpack
        }

instance D.ToDhall Conf where
  injectWith _ =
    D.genericToDhallWith $
      D.defaultInterpretOptions
        { D.fieldModifier = T.pack . Case.camel . drop 1 . T.unpack
        }

main :: IO ()
main = do
  (confPath, cacheDir) <-
    O.execParser $
      O.info
        ( ( (,)
              <$> O.strOption
                (O.long "conf" <> O.metavar "PATH" <> O.value "santabot-irc-conf.dhall" <> O.showDefault)
              <*> O.strOption
                (O.long "cache" <> O.metavar "PATH" <> O.value "cache" <> O.showDefault)
          )
            <**> O.helper
        )
        ( O.fullDesc
            <> O.progDesc "santabot irc client"
            <> O.header "santabot-irc -- santabot irc client"
        )
  c@Conf{..} <- D.inputFile D.auto confPath
  PP.putDoc $ D.prettyExpr (D.embed D.inject c)
  putStrLn ""
  mgr <- newTlsManager
  intcodeMap <- newIORef mempty
  launchIRC
    cServer
    cChannels
    cNick
    cPassword
    cSasl
    (fromIntegral cTick * 1000000)
    (masterBot cBotConf mgr intcodeMap cacheDir)
