module Comparison exposing (..)
import Html exposing (Html, button, div, text, span, table, tr, th, td)
import Html.Attributes exposing(class)
import Html.App as App
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing (..)
import Task
import Model exposing (..)

main =
    App.program { init = ( initialModel, Cmd.none ), view = view, update = update, subscriptions = \_ -> Sub.none }

-- MODEL

type alias Entries = {leftEntries : List GogEntry, rightEntries : List SteamEntry}

type alias Model = {leftEntries : List GogEntry, rightEntries : List SteamEntry, message : String}

initialModel = {leftEntries = [], rightEntries = [], message = "Click to refresh"}

-- UPDATE

type Msg
  = SendRefresh String
  | ReceiveRefresh Entries
  | RefreshError Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SendRefresh address ->
      (model, getResponse address)
    ReceiveRefresh entries ->
      ({model | leftEntries = entries.leftEntries, rightEntries = entries.rightEntries} , Cmd.none)
    RefreshError err ->
      ({model | leftEntries = [], rightEntries = [], message = toString err} , Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div [] <|
    [ button [ onClick <| SendRefresh "comparison/data"] [ text "Fetch data"   ]
    , div [] [ text (toString model.message) ]
    , div [] [ table[class <| "inlineTable"] <| gogTableTitle :: (List.map gogTableRow model.leftEntries)
             , table[class <| "inlineTable"] <| steamTableTitle :: (List.map steamTableRow model.rightEntries)
             ]
    ]

gogTableTitle = tr [] [ th[][text "Gog Game"] ]

gogTableRow e = tr [] [ td[][text e.title] ]

steamTableTitle = tr [] [ th[][text "Steam Game"] ]

steamTableRow e = tr [] [ td[][text e.name] ]


getResponse address=
  let
    url = "http://localhost:9000/" ++ address
  in
    Task.perform RefreshError ReceiveRefresh (Http.get decodeResponse url)

decodeResponse : Json.Decoder Entries
decodeResponse =
  tuple2
    Entries
    (list <| object3 GogEntry ("id" := int) ("title" := string) ("gogId" := int))
    (list <| object3 SteamEntry ("id" := int) ("name" := string) ("steamId" := int))
