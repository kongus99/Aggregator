module Router exposing(gogData, steamData, allData, toggleSelected, comparisonData, resolveResponse)

import Http
import Json.Decode as Json exposing (..)
import Model exposing (..)
import String
--METHODS
gogData params =   (Http.get (routes.home.gogData params) (list decodedGameEntry), routes.home.page params)
steamData params = (Http.get (routes.home.steamData params) (list decodedGameEntry), routes.home.page params)
allData params =   (Http.get (routes.home.allData params) (list decodedGameEntry), routes.home.page params)
toggleSelected params = Http.post (routes.comparison.toggleSelected params) Http.emptyBody string
comparisonData params = (Http.get (routes.comparison.comparisonData params) (list decodedComparisonEntry), routes.comparison.page params)
--ROUTES
baseAddress : String
baseAddress = "http://localhost:9000"

type alias UrlGenerator = List (String, String) -> String
type alias Addresses = {home : Home, comparison : Comparison}
type alias Home = {gogData : UrlGenerator, steamData : UrlGenerator, allData : UrlGenerator, page : UrlGenerator}
type alias Comparison = {toggleSelected : UrlGenerator, comparisonData : UrlGenerator, page : UrlGenerator}

home = Home (generateAddress "gogData") (generateAddress "steamData") (generateAddress "allData") (generateAddress "")
comparison = Comparison (generateAddress "comparison/toggleMatch") (generateAddress "comparison/data") (generateAddress "comparison")

routes = Addresses home comparison

--DECODERS
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
        baseAddress ++ "/" ++ resourceName ++ "?" ++ (joinParameters params)

resolveResponse successResolver errorResolver response =
    case response of
        Ok x -> successResolver x
        Err y -> errorResolver y
