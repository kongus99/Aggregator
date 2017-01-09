module Router exposing(gogData, steamData, allData, toggleSelected, comparisonData, resolveResponse, fetchUser, createUpdateUser, homePageUrl)

import Http
import Json.Decode as Json exposing (..)
import Model exposing (..)
import String
--METHODS
fetchUser params =   Http.get (routes.login.fetch params) decodedUserEntry
createUpdateUser params =  Http.post (routes.login.createUpdate params) Http.emptyBody decodedUserEntry
gogData params =   (Http.get (routes.home.gogData params) (list decodedGameEntry), homePageUrl params)
steamData params = (Http.get (routes.home.steamData params) (list decodedGameEntry), homePageUrl params)
allData params =   (Http.get (routes.home.allData params) (list decodedGameEntry), homePageUrl params)
toggleSelected params = Http.post (routes.comparison.toggleSelected params) Http.emptyBody string
comparisonData params = (Http.get (routes.comparison.comparisonData params) (list decodedComparisonEntry), routes.comparison.page params)
--ROUTES

type alias UrlGenerator = List (String, String) -> String
type alias Addresses = {login : Login, home : Home, comparison : Comparison}
type alias Login = {fetch : UrlGenerator, createUpdate : UrlGenerator}
type alias Home = {gogData : UrlGenerator, steamData : UrlGenerator, allData : UrlGenerator, page : UrlGenerator}
type alias Comparison = {toggleSelected : UrlGenerator, comparisonData : UrlGenerator, page : UrlGenerator}

login = Login (generateAddress "login/fetch") (generateAddress "login/createUpdate")
home = Home (generateAddress "gogData") (generateAddress "steamData") (generateAddress "allData") (generateAddress "")
comparison = Comparison (generateAddress "comparison/toggleMatch") (generateAddress "comparison/data") (generateAddress "comparison")

routes = Addresses login home comparison
--URLS
homePageUrl params = routes.home.page params
--DECODERS
decodedUserEntry = map3 User (field "id" (maybe int)) (field "steamLogin" (maybe string)) (field "gogLogin" (maybe string))
decodedGogEntry = map4 GogEntry (field "title" string) (field "gogId" int) (field "price" (maybe float)) (field "discounted" (maybe float))
decodedSteamEntry = map4 SteamEntry (field "name" string) (field "steamId" int) (field "price" (maybe float)) (field "discounted" (maybe float))
decodedPriceEntry = map5 PriceEntry (field "steamId" int) (field "name" string) (field "host" string) (field "link" string) (field "price" float)
decodedGameEntry = map3 GameEntry (field "gog" (list decodedGogEntry)) (field "steam" (list decodedSteamEntry)) (field "prices" (list decodedPriceEntry))
decodedComparisonEntry = map4 ComparisonEntry (field "left" decodedNamedEntry) (field "metricResult" int) (field "right" decodedNamedEntry) (field "matches" bool)
decodedNamedEntry = map2 NamedEntry (field "id" int) (field "name" string)
-- HELPERS

generateAddress resourceName params =
    let
        joinParameters params = String.join "&&" (List.map (\((k, v)) -> k ++ "=" ++ v) params)
    in
        "/" ++ resourceName ++ "?" ++ (joinParameters params)

resolveResponse successResolver errorResolver response =
    case response of
        Ok x -> successResolver x
        Err y -> errorResolver y
