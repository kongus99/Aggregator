module Router exposing(gogData, steamData, allData)

import Http
import Json.Decode as Json exposing (..)
import Model exposing (..)

prepareRequest address decoder = Http.get decoder ("http://localhost:9000/" ++ address)

gogData = prepareRequest "gogData" <| list decodedGameEntry
steamData = prepareRequest "steamData" <| list decodedGameEntry
allData = prepareRequest "allData" <| list decodedGameEntry

decodedGogEntry = object2 GogEntry ("title" := string) ("gogId" := int)
decodedSteamEntry = object2 SteamEntry ("name" := string) ("steamId" := int)
decodedGameEntry = object2 GameEntry ("gog" := (list decodedGogEntry)) ("steam" := (list decodedSteamEntry))