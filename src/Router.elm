module Router exposing (routes, MethodGenerator, resolveResponse, refreshSocketUrl, decodeWebSocketResult)

import Http
import Json.Decode as Json exposing (..)
import Model exposing (..)
import GameEntry exposing (GameEntry, WebSocketRefreshResult)
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


routes =
    { login =
        { fetchUsers = (generateGetMethod "login/fetchUsers" (list decodedUserEntry))
        , createUpdate = (generatePostMethod "login/createUpdate" decodedUserEntry)
        , steamAlternate = (generatePostMethod "login/steamAlternate" decodedUserEntry)
        }
    , main =
        { fetch = (generateGetMethod "main/fetch" (list decodedGameEntry))
        , page = (generateGetMethod "main" string)
        }
    , gameOptions =
        { fetch = (generateGetMethod "gameOptions/fetch" decodedGameOptionsEntry)
        , changeSelectedSearch = (generatePostMethod "gameOptions/changeSelectedSearch" string)
        , fetchSearchResults = (generateGetMethod "gameOptions/fetchSearchResults" decodedGameOptionsEntry)
        , triggerRefresh = (generatePostMethod "gameOptions/refresh" string)
        }
    , comparison =
        { toggleSelected = (generatePostMethod "comparison/toggleMatch" string)
        , comparisonData = (generateGetMethod "comparison/data" (list decodedComparisonEntry))
        , page = (generateGetMethod "comparison" string)
        }
    }



--WEB SOCKET


refreshSocketUrl : String -> String
refreshSocketUrl host =
    "ws://" ++ host ++ "/refreshsocket"


decodeWebSocketResult : String -> WebSocketRefreshResult
decodeWebSocketResult r =
    WebSocketRefreshResult
        (r |> Json.decodeString (list decodedGameEntry) |> Result.toMaybe)
        (r |> Json.decodeString (list decodedPriceEntry) |> Result.toMaybe)



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
