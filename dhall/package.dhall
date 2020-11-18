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
    }
