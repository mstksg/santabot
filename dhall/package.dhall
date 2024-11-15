let types = ./types.dhall

let CommandBot = types.CommandBot

let ChallengeEvent = types.ChallengeEvent

let AlertBot = types.AlertBot

let LeaderboardInfo = types.LeaderboardInfo

in  { types
    , allChallengeEvents =
      [ ChallengeEvent.Hour
      , ChallengeEvent.TenMin
      , ChallengeEvent.Minute
      , ChallengeEvent.Start
      ]
    , defaultCommandBots =
        λ(about : Text) →
        λ(lbi : Optional LeaderboardInfo) →
            [ CommandBot.PuzzleLink
            , CommandBot.PuzzleThread
            , CommandBot.CapTimeBot
            , CommandBot.NextPuzzle
            , CommandBot.Intcode
            , CommandBot.About about
            , CommandBot.Time
            ]
          # merge
              { Some = λ(i : LeaderboardInfo) → [ CommandBot.Leaderboard i ]
              , None = [] : List CommandBot
              }
              lbi
    , defaultAlertBots =
        λ(ce : List ChallengeEvent) →
        λ(limit : Optional Natural) →
        λ ( mpc
          : Optional
              { session : Text
              , leaderboardInfo : LeaderboardInfo
              , cap : Natural
              }
          ) →
            [ AlertBot.ChallengeCountdown ce
            , AlertBot.EventCountdown limit
            , AlertBot.BoardCapped
            ]
          # merge
              { Some =
                  λ ( pc
                    : { session : Text
                      , leaderboardInfo : LeaderboardInfo
                      , cap : Natural
                      }
                    ) →
                    [ AlertBot.PrivateCapped pc ]
              , None = [] : List AlertBot
              }
              mpc
    , sourceRepo = "https://github.com/mstksg/santabot"
    , showLeaderboardInfo =
        λ(li : { leaderboard : Natural, joinCode : Optional Text }) →
          let jcstr =
                merge { Some = λ(c : Text) → "-${c}", None = "" } li.joinCode

          in  "${Natural/show li.leaderboard}${jcstr}"
    , showLeaderboardLink =
        λ(year : Natural) →
        λ(leaderboard : Natural) →
          let sy = Natural/show year

          let sl = Natural/show leaderboard

          in  "https://adventofcode.com/${sy}/leaderboard/private/view/${sl}"
    , defaultPhrasebook =
      [ "Ho ho ho!"
      , "Deck the channels!"
      , "Joy to the libera!"
      , "I just elfed myself!"
      , "Have you been naughty or nice?"
      , "Won't you guide my debugger tonight?"
      , "Calling all reindeer!"
      , "Run that checksum twice!"
      , "Yule be in for a treat!"
      , "A round of Santa-plause, please."
      , "Let it snow!"
      , "Jingle \\a, jingle all the way!"
      , "Hoooo ho ho!"
      , "Do you hear what I hear?"
      , "Who's ready for Christmas cheer?"
      , "Get out the hot cocoa!"
      , "I'm dreaming of a bug-free Christmas!"
      , "My list is formally verified twice!"
      , "Now Dasher, now Dancer!"
      , "It's the most wonderful time of the year!"
      , "Francisco!"
      , "Son of a nutcracker!"
      , "Has anyone seen my sleigh?"
      , "Sharpen up your elfcode!"
      , "Sing loud for all to hear!"
      , "Time to spread some Christmas cheer!"
      , "It's silly, but I believe!"
      , "Yipee ki-yay, coders!"
      , "Welcome to the party, pal."
      , "I'll be \$HOME for Christmas!"
      , "Hark!"
      , "Hooo ho ho!"
      , "Ho ho hoooo!"
      , "Rest ye merry CPUs!"
      , "Code yourself a merry little Christmas!"
      , "Must've been some magic in that old compiler!"
      ]
    }
