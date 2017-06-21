module Router exposing (MethodGenerator, decodeWebSocketResult, refreshSocketUrl, resolveResponse, routes)

import Erl
import GameEntry exposing (GameEntry, WebSocketRefreshResult)
import Http
import Json.Decode as Json exposing (..)
import Model exposing (..)
import Parser
import Price exposing (AlternatePrice, Price)
import String


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


routes =
    { login =
        { page = generateGetMethod "" string
        , fetchUsers = generateGetMethod "login/fetchUsers" (list decodedUserEntry)
        , createUpdate = generatePostMethod "login/createUpdate" decodedUserEntry
        , steamAlternate = generatePostMethod "login/steamAlternate" decodedUserEntry
        }
    , main =
        { fetch = generateGetMethod "main/fetch" (list decodedGameEntry)
        , page = generateGetMethod "main" string
        }
    , gameOptions =
        { fetch = generateGetMethod "gameOptions/fetch" decodedGameOptionsEntry
        , changeSelectedSearch = generatePostMethod "gameOptions/changeSelectedSearch" string
        , fetchSearchResults = generateGetMethod "gameOptions/fetchSearchResults" decodedGameOptionsEntry
        , triggerRefresh = generatePostMethod "gameOptions/refresh" string
        }
    , comparison =
        { toggleSelected = generatePostMethod "comparison/toggleMatch" string
        , comparisonData = generateGetMethod "comparison/data" (list decodedComparisonEntry)
        , page = generateGetMethod "comparison" string
        }
    }



--WEB SOCKET


refreshSocketUrl : Protocol -> String -> Int -> String
refreshSocketUrl protocol host userId =
    if protocol == Https then
        "wss://" ++ host ++ "/refreshsocket/" ++ toString userId
    else
        "ws://" ++ host ++ "/refreshsocket/" ++ toString userId


decodeWebSocketResult : String -> WebSocketRefreshResult
decodeWebSocketResult r =
    WebSocketRefreshResult
        (r |> Json.decodeString (map2 (,) (index 0 (list decodedGameEntry)) (index 1 (list decodedGameEntry))) |> Result.toMaybe)
        (r |> Json.decodeString (map2 (,) (index 0 (list int)) (index 1 (list decodedAlternatePrice))) |> Result.toMaybe)



--DECODERS


csvRowDecoder separator stringDecoder =
    map (\s -> String.split separator s) stringDecoder


decodedUserEntry =
    map4 User (field "id" (maybe int)) (field "steamLogin" (maybe string)) (field "steamAlternate" bool) (field "gogLogin" (maybe string))


decodedGogEntry =
    map5 GogEntry
        (field "title" string)
        (field "link" string)
        (field "gogId" int)
        (maybe (field "price" decodedPrice))
        (field "genres" (csvRowDecoder "," string))


decodedSteamEntry =
    map6 SteamEntry
        (field "name" string)
        (field "steamId" int)
        (field "link" string)
        (maybe (field "price" decodedPrice))
        (field "genres" (csvRowDecoder "," string))
        (field "tags" (csvRowDecoder "," string))


decodedPrice =
    map2 Price (field "normal" float) (maybe (field "discounted" float))


decodedAlternatePrice =
    map5 AlternatePrice (field "steamId" int) (field "name" string) (field "host" string) (field "link" string) (field "price" float)


decodedGameEntry =
    map3 GameEntry (field "gog" (list decodedGogEntry)) (field "steam" (list decodedSteamEntry)) (field "prices" (list decodedAlternatePrice))


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
            Erl.addQuery k v u
    in
    List.foldl folder defaultUrl params |> Erl.toString


resolveResponse : (a -> c) -> (b -> c) -> Result b a -> c
resolveResponse successResolver errorResolver response =
    case response of
        Ok x ->
            successResolver x

        Err y ->
            errorResolver y
