{ name : Text
, author : Text
, home : Text
, alerts : Text
, session : Optional Text
, leaderboard : Optional Natural
, joinCode : Optional Text
, countdown : Optional Natural
, commandBots : List ./CommandBot.dhall
, alertBots : List ./AlertBot.dhall
, phrasebook : List Text
}
