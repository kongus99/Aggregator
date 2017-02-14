module Model exposing (..)

import Array exposing (Array)


type GameOn
    = Gog
    | Steam


type GameSources
    = Owned
    | WishList
    | Both


type alias SteamUsername =
    String


type alias GogUserName =
    String


type alias User =
    { id : Maybe Int, username1 : Maybe SteamUsername, steamAlternate : Bool, username2 : Maybe GogUserName }


type alias GogEntry =
    { title : String, gogId : Int, price : Maybe Float, discounted : Maybe Float }


type alias SteamEntry =
    { name : String, steamId : Int, price : Maybe Float, discounted : Maybe Float }


type alias PriceEntry =
    { steamId : Int, name : String, host : String, link : String, price : Float }


type alias NamedEntry =
    { id : Int, name : String }


type alias ComparisonEntry =
    { left : NamedEntry, metricResult : Int, right : NamedEntry, matches : Bool }


type alias GameQuery =
    { query : String, site : String, results : Array String, selectedResult : Int }


type alias GameOptions =
    { entry : SteamEntry, queries : Array GameQuery }
