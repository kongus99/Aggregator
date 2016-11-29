module Model exposing (..)


type GameOn = Gog | Steam

type alias GameEntry = {gog: List GogEntry, steam: List SteamEntry}

type alias GogEntry = {title: String, gogId : Int}

type alias SteamEntry = {name: String, steamId : Int }

type alias NamedEntry = {id : Int, name : String}

type alias ComparisonEntry = {left : NamedEntry, metricResult : Int, right : NamedEntry, matches : Bool}



