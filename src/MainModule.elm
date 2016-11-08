module MainModule exposing (..)
import Html exposing (Html, button, div, text, span, table, tr, th, td)
import Html.Attributes exposing(class)
import Html.App as App
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing (..)
import Task

main =
    App.program { init = ( initialModel, Cmd.none ), view = view, update = update, subscriptions = \_ -> Sub.none }

-- MODEL

type GameOn = Gog | Steam

type alias GameEntry = {name : String, gameOn : List GameOn}

type alias Model = {entries : List GameEntry, message : String}

initialModel : Model
initialModel = {entries = [], message = "Click to refresh"}

-- UPDATE

type Msg
  = SendRefresh String
  | ReceiveRefresh (List GameEntry)
  | RefreshError Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SendRefresh address ->
      (model, getResponse address)
    ReceiveRefresh entries ->
      ({model | entries = entries} , Cmd.none)
    RefreshError err ->
      ({model | entries = [], message = toString err} , Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div [] <|
    [ button [ onClick <| SendRefresh "gogData"] [ text "Fetch from gog"   ]
    , button [ onClick <| SendRefresh "steamData"] [ text "Fetch from steam" ]
    , div [] [ text (toString model.message) ]
    , table[] <| gameTableTitle :: (List.map gameTableRow model.entries)
    ]

gameTableTitle =
    tr [] [ th[][text "Game"]
          , th[][text "Gog/Steam/Both"]
          ]

gameTableRow e =
    tr [] [ td[][text e.name]
          , td[class <| toStyle e.gameOn  ](toText e.gameOn)
          ]

getResponse address=
  let
    url = "http://localhost:9000/" ++ address
  in
    Task.perform RefreshError ReceiveRefresh (Http.get decodeResponse url)

decodeResponse : Json.Decoder (List GameEntry)
decodeResponse =
  list (object2 GameEntry
                ("name" := string)
                (map gamesOn ("on" := list string))
                )

gamesOn list = List.map mapSingle list

mapSingle e = if e == "Gog" then Gog else Steam

toStyle gamesOn =
    let
        onGog = List.member Gog gamesOn
        onSteam = List.member Steam gamesOn
    in
        if onGog && onSteam then "cell_Both" else if onGog then "cell_Gog" else "cell_Steam"
toText gamesOn =
    let
        onGogNumber = List.filter (\g -> g == Gog) gamesOn |> List.length
        onSteamNumber = List.filter (\g -> g == Steam) gamesOn |> List.length
        onGogSpan = toSpan onGogNumber "gog_number"
        onSteamSpan = toSpan onSteamNumber "steam_number"
    in
        [onGogSpan, onSteamSpan]
toSpan n styleClass =
    if n > 1 then span[class styleClass ][text <| toString n] else span[][]