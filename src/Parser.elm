module Parser exposing (..)

import Model exposing (..)


parseInt : String -> Maybe Int
parseInt value =
    String.toInt value |> Result.toMaybe


parseFloat : String -> Maybe Float
parseFloat value =
    String.toFloat value |> Result.toMaybe


parseBool : String -> Maybe Bool
parseBool value =
    case String.toLower value of
        "true" ->
            Just True

        "false" ->
            Just False

        _ ->
            Nothing


parseSources : String -> Maybe GameSources
parseSources value =
    case value of
        "Owned" ->
            Just Owned

        "WishList" ->
            Just WishList

        "Both" ->
            Just Both

        _ ->
            Nothing


parseGameOn : String -> Maybe GameOn
parseGameOn value =
    case value of
        "Gog" ->
            Just Gog

        "Steam" ->
            Just Steam

        _ ->
            Nothing
