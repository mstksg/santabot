< ChallengeCountdown : List ./ChallengeEvent.dhall
| EventCountdown : Optional Natural
| BoardCapped
| PrivateCapped :
    { session : Text, leaderboardInfo : ./LeaderboardInfo.dhall, cap : Natural }
>
