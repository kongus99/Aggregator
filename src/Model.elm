module Model exposing (..)

import Array exposing (Array)
import Price exposing (Price)


type GameOn
    = Gog
    | Steam


type GameSources
    = Owned
    | WishList
    | Both


type Protocol
    = Http
    | Https
    | Other


type alias SteamUsername =
    String


type alias GogUserName =
    String


type alias User =
    { id : Maybe Int, steamUsername : Maybe SteamUsername, steamAlternate : Bool, gogUsername : Maybe GogUserName }


type alias GogEntry =
    { title : String, link : String, gogId : Int, price : Maybe Price, genres : List String }


type alias SteamEntry =
    { name : String, steamId : Int, link : String, price : Maybe Price, genres : List String, tags : List String }


type alias NamedEntry =
    { id : Int, name : String }


type alias ComparisonEntry =
    { left : NamedEntry, metricResult : Int, right : NamedEntry, matches : Bool }


type alias GameQuery =
    { query : String, site : String, results : List String, selectedResult : Maybe String }


type alias GameOptions =
    { entry : SteamEntry, queries : Array GameQuery }
