{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.IORef
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Dhall as D
import qualified Dhall.Pretty as D
import GHC.Generics
import Main.Master
import Network.HTTP.Client.TLS
import Numeric.Natural
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
  c@Conf{..} <- D.inputFile D.auto "santabot-conf-irc.dhall"
  PP.putDoc $ D.prettyExpr (D.embed D.inject c)
  putStrLn ""
  phrasebook <- S.fromList . map T.pack . lines <$> readFile "phrasebook.txt"
  mgr <- newTlsManager
  intcodeMap <- newIORef mempty
  launchIRC
    cServer
    cChannels
    cNick
    cPassword
    cSasl
    (fromIntegral cTick * 1000000)
    (masterBot cBotConf mgr intcodeMap phrasebook)
