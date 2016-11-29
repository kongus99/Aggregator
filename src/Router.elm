module Router exposing(gogData, steamData, allData, toggleSelected, comparisonData)

import Http
import Json.Decode as Json exposing (..)
import Model exposing (..)
import String
--PUBLIC
gogData = prepareRequest "gogData" <| list decodedGameEntry
steamData = prepareRequest "steamData" <| list decodedGameEntry
allData = prepareRequest "allData" <| list decodedGameEntry
toggleSelected decoder params = Http.post decoder (url params) Http.empty
comparisonData params = (Http.get (list decodedComparisonEntry) (dataUrl params), pageUrl params)
--PRIVATE

prepareRequest address decoder = Http.get decoder ("http://localhost:9000/" ++ address)

decodedGogEntry = object2 GogEntry ("title" := string) ("gogId" := int)
decodedSteamEntry = object2 SteamEntry ("name" := string) ("steamId" := int)
decodedGameEntry = object2 GameEntry ("gog" := (list decodedGogEntry)) ("steam" := (list decodedSteamEntry))

decodedComparisonEntry = object4 ComparisonEntry ("left" := decodedNamedEntry) ("metricResult" := int) ("right" := decodedNamedEntry) ("matches" := bool)
decodedNamedEntry = object2 NamedEntry ("id" := int) ("name" := string)


dataUrl params = "http://localhost:9000/comparison/data" ++ "?" ++ (joinParameters params)
pageUrl params = "http://localhost:9000/comparison" ++ "?" ++ (joinParameters params)
url params = "http://localhost:9000/comparison/toggleMatch" ++ "?" ++ (joinParameters params)


joinParameters params =
    String.join "&&" (List.map (\((k, v)) -> k ++ "=" ++ v) params)