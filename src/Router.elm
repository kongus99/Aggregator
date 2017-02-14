module Router exposing (refreshUserGames, getUserGames, toggleSelected, comparisonData, resolveResponse, fetchUser, createUpdateUser, updateSteamAlternate, mainPageUrl, refreshSocketUrl, fetchGameOptions)

import Http
import Json.Decode as Json exposing (..)
import Model exposing (..)
import GameEntry exposing (GameEntry)
import String


--METHODS


fetchUser params =
    Http.get (routes.login.fetch params) decodedUserEntry


createUpdateUser params =
    Http.post (routes.login.createUpdate params) Http.emptyBody decodedUserEntry


updateSteamAlternate params =
    Http.post (routes.login.steamAlternate params) Http.emptyBody decodedUserEntry


refreshUserGames params =
    ( Http.get (routes.main.refreshGames params) (list decodedGameEntry), routes.main.page params )


getUserGames params =
    ( Http.get (routes.main.fetch params) (list decodedGameEntry), routes.main.page params )


fetchGameOptions params =
    Http.get (routes.main.gameOptions params) decodedGameOptionsEntry


toggleSelected params =
    Http.post (routes.comparison.toggleSelected params) Http.emptyBody string


comparisonData params =
    ( Http.get (routes.comparison.comparisonData params) (list decodedComparisonEntry), routes.comparison.page params )



--ROUTES


type alias UrlGenerator =
    List ( String, String ) -> String


type alias Addresses =
    { login : Login, main : Main, comparison : Comparison }


type alias Login =
    { fetch : UrlGenerator, createUpdate : UrlGenerator, steamAlternate : UrlGenerator }


type alias Main =
    { refreshGames : UrlGenerator, gameOptions : UrlGenerator, fetch : UrlGenerator, page : UrlGenerator }


type alias Comparison =
    { toggleSelected : UrlGenerator, comparisonData : UrlGenerator, page : UrlGenerator }


login =
    Login (generateAddress "login/fetch") (generateAddress "login/createUpdate") (generateAddress "login/steamAlternate")


main_ =
    Main (generateAddress "main/refresh") (generateAddress "main/gameOptions") (generateAddress "main/fetch") (generateAddress "main")


comparison =
    Comparison (generateAddress "comparison/toggleMatch") (generateAddress "comparison/data") (generateAddress "comparison")


routes =
    Addresses login main_ comparison



--URLS


mainPageUrl =
    "/main"


refreshSocketUrl host =
    "ws://" ++ host ++ "/refreshSocket"



--DECODERS


decodedUserEntry =
    map4 User (field "id" (maybe int)) (field "steamLogin" (maybe string)) (field "steamAlternate" (bool)) (field "gogLogin" (maybe string))


decodedGogEntry =
    map4 GogEntry (field "title" string) (field "gogId" int) (field "price" (maybe float)) (field "discounted" (maybe float))


decodedSteamEntry =
    map4 SteamEntry (field "name" string) (field "steamId" int) (field "price" (maybe float)) (field "discounted" (maybe float))


decodedPriceEntry =
    map5 PriceEntry (field "steamId" int) (field "name" string) (field "host" string) (field "link" string) (field "price" float)


decodedGameEntry =
    map3 GameEntry (field "gog" (list decodedGogEntry)) (field "steam" (list decodedSteamEntry)) (field "prices" (list decodedPriceEntry))


decodedComparisonEntry =
    map4 ComparisonEntry (field "left" decodedNamedEntry) (field "metricResult" int) (field "right" decodedNamedEntry) (field "matches" bool)


decodedNamedEntry =
    map2 NamedEntry (field "id" int) (field "name" string)


decodedGameOptionsEntry =
    map2 GameOptions (field "entry" decodedSteamEntry) (field "queries" (list decodedGameQueryEntry))


decodedGameQueryEntry =
    map3 GameQuery (field "query" string) (field "site" string) (field "results" (list string))



-- HELPERS


generateAddress resourceName params =
    let
        joinParameters params =
            String.join "&&" (List.map (\( k, v ) -> k ++ "=" ++ v) params)
    in
        "/" ++ resourceName ++ "?" ++ (joinParameters params)


resolveResponse : (a -> c) -> (b -> c) -> Result b a -> c
resolveResponse successResolver errorResolver response =
    case response of
        Ok x ->
            successResolver x

        Err y ->
            errorResolver y
