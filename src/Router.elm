module Router exposing(gogData, steamData, allData, toggleSelected, comparisonData)

import Http
import Json.Decode as Json exposing (..)
import Model exposing (..)
import String
--METHODS
gogData params =   Http.get (list decodedGameEntry) (routes.main.gogData params)
steamData params = Http.get (list decodedGameEntry) (routes.main.steamData params)
allData params =   Http.get (list decodedGameEntry) (routes.main.allData params)
toggleSelected params = Http.post string (routes.comparison.toggleSelected params) Http.empty
comparisonData params = (Http.get (list decodedComparisonEntry) (routes.comparison.comparisonData params), routes.comparison.page params)
--ROUTES
baseAddress : String
baseAddress = "http://localhost:9000"

type alias Addresses = {main : Home, comparison : Comparison}
type alias Home = {gogData : List (String, String) -> String, steamData : List (String, String) -> String, allData : List (String, String) -> String}
type alias Comparison = {toggleSelected : List (String, String) -> String, comparisonData : List (String, String) -> String, page : List (String, String) -> String}

home = Home (\params -> baseAddress ++ "/gogData?" ++ (joinParameters params))
            (\params -> baseAddress ++ "/steamData?" ++ (joinParameters params))
            (\params -> baseAddress ++ "/allData?" ++ (joinParameters params))
comparison = Comparison (\params -> baseAddress ++ "/comparison/toggleMatch?" ++ (joinParameters params))
                        (\params -> baseAddress ++ "/comparison/data?" ++ (joinParameters params))
                        (\params -> baseAddress ++ "/comparison?" ++ (joinParameters params))
routes = Addresses home comparison

--DECODERS
decodedGogEntry = object2 GogEntry ("title" := string) ("gogId" := int)
decodedSteamEntry = object2 SteamEntry ("name" := string) ("steamId" := int)
decodedGameEntry = object2 GameEntry ("gog" := (list decodedGogEntry)) ("steam" := (list decodedSteamEntry))
decodedComparisonEntry = object4 ComparisonEntry ("left" := decodedNamedEntry) ("metricResult" := int) ("right" := decodedNamedEntry) ("matches" := bool)
decodedNamedEntry = object2 NamedEntry ("id" := int) ("name" := string)
-- HELPERS
joinParameters params =
    String.join "&&" (List.map (\((k, v)) -> k ++ "=" ++ v) params)