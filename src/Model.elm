module Model exposing (..)


type GameOn = Gog | Steam

type alias GameEntry = {gog: List GogEntry, steam: List SteamEntry, prices : List PriceEntry}

type alias GogEntry = {title: String, gogId : Int, price : Maybe Float, discounted : Maybe Float}

type alias SteamEntry = {name: String, steamId : Int, price : Maybe Float, discounted : Maybe Float }

type alias PriceEntry = {steamId: Int, name : String, host : String, link: String, price: Float}

type alias NamedEntry = {id : Int, name : String}

type alias ComparisonEntry = {left : NamedEntry, metricResult : Int, right : NamedEntry, matches : Bool}



