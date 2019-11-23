
import           Elfbot
import           Elfbot.Bot
import           Elfbot.Run

masterBot :: Bot IO ()
masterBot = mergeBots
  [ commandBot eventLink
  , alertBot challengeCountdown
  , alertBot eventCountdown
  -- , alertBot acknowledgeTick
  ]

main :: IO ()
main = launchIRC ["##elfbot-test"] "elfbot" 5000000 masterBot
