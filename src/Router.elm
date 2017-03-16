module Router exposing (routes, MethodGenerator, getUserGames, toggleSelected, comparisonData, resolveResponse, mainPageUrl, refreshSocketUrl, fetchGameOptions, saveSelectedSearchResult, fetchNewSearchResults)

import Http exposing (Request)
import Json.Decode as Json exposing (..)
import Model exposing (..)
import GameEntry exposing (GameEntry)
import String
import Erl


--METHODS


getUserGames params =
    Http.get (routes.main.fetch params) (list decodedGameEntry)


fetchGameOptions params =
    Http.get (routes.gameOptions.fetch params) decodedGameOptionsEntry


fetchNewSearchResults params =
    Http.get (routes.gameOptions.fetchSearchResults params) decodedGameOptionsEntry


saveSelectedSearchResult params =
    Http.post (routes.gameOptions.changeSelectedSearch params) Http.emptyBody string


toggleSelected params =
    Http.post (routes.comparison.toggleSelected params) Http.emptyBody string


comparisonData params =
    ( Http.get (routes.comparison.comparisonData params) (list decodedComparisonEntry), routes.comparison.page params )



--ROUTES


type alias UrlGenerator =
    List ( String, String ) -> String


type alias MethodGenerator a =
    List ( String, String ) -> Request a


type alias Addresses =
    { login : Login, main : Main, gameOptions : Options, comparison : Comparison }


type alias Login =
    { fetchUsers : MethodGenerator (List User), createUpdate : MethodGenerator User, steamAlternate : MethodGenerator User }


type alias Main =
    { fetch : UrlGenerator, page : UrlGenerator }


type alias Options =
    { fetch : UrlGenerator, changeSelectedSearch : UrlGenerator, fetchSearchResults : UrlGenerator }


type alias Comparison =
    { toggleSelected : UrlGenerator, comparisonData : UrlGenerator, page : UrlGenerator }


login =
    Login
        (\x -> Http.get (generateAddress "login/fetchUsers" x) (list decodedUserEntry))
        (\x -> Http.post (generateAddress "login/createUpdate" x) Http.emptyBody decodedUserEntry)
        (\x -> Http.post (generateAddress "login/steamAlternate" x) Http.emptyBody decodedUserEntry)


main_ =
    Main (generateAddress "main/fetch") (generateAddress "main")


gameOptions =
    Options (generateAddress "gameOptions/fetch") (generateAddress "gameOptions/changeSelectedSearch") (generateAddress "gameOptions/fetchSearchResults")


comparison =
    Comparison (generateAddress "comparison/toggleMatch") (generateAddress "comparison/data") (generateAddress "comparison")


routes =
    Addresses login main_ gameOptions comparison



--URLS


mainPageUrl : List ( String, String ) -> String
mainPageUrl params =
    routes.main.page params


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
