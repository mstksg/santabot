< ChallengeCountdown : List ./ChallengeEvent.dhall
| EventCountdown : (Natural -> Optional Text)
| BoardCapped
| PrivateCapped :
    { session : Text, leaderboardInfo : ./LeaderboardInfo.dhall, cap : Natural }
>
