let types = ./types.dhall

let CommandBot = types.CommandBot

let ChallengeEvent = types.ChallengeEvent

let AlertBot = types.AlertBot

let Quotient = ./util/quotient.dhall

let LeaderboardInfo = types.LeaderboardInfo

let lessThanEqual =
      λ(x : Natural) → λ(y : Natural) → Natural/isZero (Natural/subtract y x)

let lessThan = λ(x : Natural) → λ(y : Natural) → lessThanEqual y x == False

let equal
    : Natural → Natural → Bool
    = λ(a : Natural) → λ(b : Natural) → lessThanEqual a b && lessThanEqual b a

let powerOfTwo =
      let PotHelp =
            < Search : { pow : Natural, val : Natural }
            | Found : Natural
            | Fail
            >

      in  λ(n : Natural) →
            merge
              { Search = λ(pv : { pow : Natural, val : Natural }) → None Natural
              , Found = λ(v : Natural) → Some v
              , Fail = None Natural
              }
              ( Natural/fold
                  25
                  PotHelp
                  ( λ(acc : PotHelp) →
                      merge
                        { Search =
                            λ(pv : { pow : Natural, val : Natural }) →
                              if    lessThan n pv.val
                              then  PotHelp.Fail
                              else  if equal n pv.val
                              then  PotHelp.Found pv.pow
                              else  PotHelp.Search
                                      { pow = pv.pow + 1, val = pv.val * 2 }
                        , Found = λ(v : Natural) → PotHelp.Found v
                        , Fail = PotHelp.Fail
                        }
                        acc
                  )
                  (PotHelp.Search { pow = 0, val = 1 })
              )

let exactDays =
      λ(sec : Natural) →
        let qr = Quotient.quotient sec 86400

        in  if Natural/isZero qr.r then Some qr.q else None Natural

in  { types
    , util = { exactDays, powerOfTwo }
    , allChallengeEvents =
      [ ChallengeEvent.Hour
      , ChallengeEvent.TenMin
      , ChallengeEvent.Minute
      , ChallengeEvent.Start
      ]
    , countdownConditions =
      { numDays =
          λ(lim : Natural) →
          λ(n : Natural) →
            merge
              { Some =
                  λ(d : Natural) →
                    if    lessThanEqual d lim
                    then  Some (Natural/show d ++ " days")
                    else  None Text
              , None = None Text
              }
              (exactDays n)
      , pow2Days =
          λ(lim : Natural) →
          λ(n : Natural) →
            merge
              { Some =
                  λ(d : Natural) →
                    if    lessThanEqual d lim
                    then  merge
                            { Some =
                                λ(p : Natural) →
                                  Some ("2^" ++ Natural/show p ++ " days")
                            , None = None Text
                            }
                            (powerOfTwo d)
                    else  None Text
              , None = None Text
              }
              (exactDays n)
      , pow2Secs =
          λ(limSecs : Natural) →
          λ(n : Natural) →
            if    lessThanEqual n limSecs
            then  merge
                    { Some =
                        λ(p : Natural) →
                          Some ("2^" ++ Natural/show p ++ " seconds")
                    , None = None Text
                    }
                    (powerOfTwo n)
            else  None Text
      }
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
        λ(limit : Natural → Optional Text) →
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
