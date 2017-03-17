module Router exposing (routes, MethodGenerator, resolveResponse, refreshSocketUrl)

import Http
import Json.Decode as Json exposing (..)
import Model exposing (..)
import GameEntry exposing (GameEntry)
import String
import Erl


--ROUTES


type alias UrlGenerator =
    List ( String, String ) -> String


type alias MethodGenerator a =
    List ( String, String ) -> { url : String, request : Http.Request a }


type alias Addresses =
    { login : Login, main : Main, gameOptions : Options, comparison : Comparison }


type alias Login =
    { fetchUsers : MethodGenerator (List User), createUpdate : MethodGenerator User, steamAlternate : MethodGenerator User }


type alias Main =
    { fetch : MethodGenerator (List GameEntry), page : MethodGenerator String }


type alias Options =
    { fetch : MethodGenerator GameOptions, changeSelectedSearch : MethodGenerator String, fetchSearchResults : MethodGenerator GameOptions }


type alias Comparison =
    { toggleSelected : MethodGenerator String, comparisonData : MethodGenerator (List ComparisonEntry), page : MethodGenerator String }


login =
    Login
        (generateGetMethod "login/fetchUsers" (list decodedUserEntry))
        (generatePostMethod "login/createUpdate" decodedUserEntry)
        (generatePostMethod "login/steamAlternate" decodedUserEntry)


main_ =
    Main
        (generateGetMethod "main/fetch" (list decodedGameEntry))
        (generateGetMethod "main" string)


gameOptions =
    Options
        (generateGetMethod "gameOptions/fetch" decodedGameOptionsEntry)
        (generatePostMethod "gameOptions/changeSelectedSearch" string)
        (generateGetMethod "gameOptions/fetchSearchResults" decodedGameOptionsEntry)


comparison =
    Comparison
        (generatePostMethod "comparison/toggleMatch" string)
        (generateGetMethod "comparison/data" (list decodedComparisonEntry))
        (generateGetMethod "comparison" string)


routes =
    Addresses login main_ gameOptions comparison



--URLS


refreshSocketUrl : String -> String
refreshSocketUrl host =
    "ws://" ++ host ++ "/refreshsocket"



--DECODERS


decodedUserEntry =
    map4 User (field "id" (maybe int)) (field "steamLogin" (maybe string)) (field "steamAlternate" (bool)) (field "gogLogin" (maybe string))


decodedGogEntry =
    map5 GogEntry (field "title" string) (field "link" string) (field "gogId" int) (field "price" (maybe float)) (field "discounted" (maybe float))


decodedSteamEntry =
    map5 SteamEntry (field "name" string) (field "steamId" int) (field "link" string) (field "price" (maybe float)) (field "discounted" (maybe float))


decodedPriceEntry =
    map5 PriceEntry (field "steamId" int) (field "name" string) (field "host" string) (field "link" string) (field "price" float)


decodedGameEntry =
    map3 GameEntry (field "gog" (list decodedGogEntry)) (field "steam" (list decodedSteamEntry)) (field "prices" (list decodedPriceEntry))


decodedComparisonEntry =
    map4 ComparisonEntry (field "left" decodedNamedEntry) (field "metricResult" int) (field "right" decodedNamedEntry) (field "matches" bool)


decodedNamedEntry =
    map2 NamedEntry (field "id" int) (field "name" string)


decodedGameOptionsEntry =
    map2 GameOptions (field "entry" decodedSteamEntry) (field "queries" (array decodedGameQueryEntry))


decodedGameQueryEntry =
    map4 GameQuery (field "query" string) (field "site" string) (field "results" (list string)) (field "selectedResult" (maybe string))



-- HELPERS


generateGetMethod base decoder params =
    let
        url =
            generateAddress base params
    in
        { url = url, request = Http.get url decoder }


generatePostMethod base decoder params =
    let
        url =
            generateAddress base params
    in
        { url = url, request = Http.post url Http.emptyBody decoder }


generateAddress : String -> List ( String, String ) -> String
generateAddress resourceName params =
    let
        defaultUrl =
            Erl.parse ("/" ++ resourceName)

        folder ( k, v ) u =
            Erl.setQuery k v u
    in
        List.foldl folder defaultUrl params |> Erl.toString


resolveResponse : (a -> c) -> (b -> c) -> Result b a -> c
resolveResponse successResolver errorResolver response =
    case response of
        Ok x ->
            successResolver x

        Err y ->
            errorResolver y
