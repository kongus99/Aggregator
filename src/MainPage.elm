module MainPage exposing (..)
import Html exposing (Html, button, div, text, span, table, tr, th, td)
import Html.Attributes exposing(class)
import Html.App as App
import Html.Events exposing (onClick)
import Http
import Task
import Model exposing (..)
import Router exposing (..)


main =
    App.program { init = ( initialModel, getResponse <| Router.allData [("sources", toString initialModel.sources)]), view = view, update = update, subscriptions = \_ -> Sub.none }

-- MODEL

type GameSources = Owned | WishList | Both

type alias Model = {sources : GameSources, entries : List GameEntry, message : String}

initialModel = Model Owned [] "Click to refresh"

-- UPDATE

type Msg
  = SendRefresh (Cmd Msg)
  | ReceiveRefresh (List GameEntry)
  | RefreshError Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SendRefresh cmd -> ({model | entries = []}, cmd)
    ReceiveRefresh entries -> ({model | entries = entries} , Cmd.none)
    RefreshError err -> ({model | entries = [], message = toString err} , Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div [] <|
    [ button [ onClick <| SendRefresh <| getResponse <| Router.gogData [("sources", toString model.sources)]] [ text "Fetch from gog"   ]
    , button [ onClick <| SendRefresh <| getResponse <| Router.steamData [("sources", toString model.sources)]] [ text "Fetch from steam" ]
    , div [] [ text (toString model.message) ]
    , table[] <| gameTableTitle :: (List.map gameTableRow model.entries)
    ]

gameTableTitle =
    tr [] [ th[][text "Game"]
          , th[][text "Gog/Steam/Both"]
          ]

gameTableRow e =
    tr [] [ td[][text <| getName e]
          , td[class <| toStyle e  ](toText e)
          ]
getResponse : Platform.Task Http.Error (List GameEntry) -> Cmd Msg
getResponse httpRequest =
    Task.perform RefreshError ReceiveRefresh httpRequest

gamesOn list = List.map mapSingle list

mapSingle e = if e == "Gog" then Gog else Steam

toStyle gameEntry =
    let
        onGog = List.length gameEntry.gog > 0
        onSteam = List.length gameEntry.steam > 0
    in
        if onGog && onSteam then "cell_Both" else if onGog then "cell_Gog" else "cell_Steam"

getName gameEntry =
    let
        steamName = List.head gameEntry.steam |> Maybe.map (\g -> g.name) |> Maybe.withDefault ""
    in
        List.head gameEntry.gog |> Maybe.map (\g -> g.title) |> Maybe.withDefault steamName

toText gameEntry =
    let
        onGogNumber = List.length gameEntry.gog
        onSteamNumber = List.length gameEntry.steam
        onGogSpan = toSpan onGogNumber "gog_number"
        onSteamSpan = toSpan onSteamNumber "steam_number"
    in
        [onGogSpan, onSteamSpan]
toSpan n styleClass =
    if n > 1 then span[class styleClass ][text <| toString n] else span[][]