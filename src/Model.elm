module Model exposing (..)


type GameOn = Gog | Steam

type alias GameEntry = {name : String, gameOn : List GameOn}

type alias GogEntry = {id : Int, title: String, gogId : Int}

type alias SteamEntry = {id : Int, name: String, steamID : Int }



