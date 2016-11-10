module Comparison exposing (..)
import Html exposing (Html, button, div, text, span, table, tr, th, td)
import Html.Attributes exposing(class)
import Html.App as App
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing (..)
import Task
import String
import Model exposing (..)

main =
    App.program { init = ( initialModel, getResponse "comparison/data" ), view = view, update = update, subscriptions = \_ -> Sub.none }

-- MODEL

type alias Entries = {left : List GogEntry, right : List SteamEntry}

type alias Model = {entries : Entries, message : String}

initialModel = {entries = Entries [] [], message = ""}

-- UPDATE

type Msg
  = ReceiveData Entries
  | DataError Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceiveData entries ->
      ({model | entries = entries} , Cmd.none)
    DataError err ->
      ({model | entries = Entries [] [], message = toString err} , Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div [] <|
    [ div [] (if String.isEmpty model.message then [] else [ text (toString model.message) ])
    , div [] [ table[class <| "inlineTable"] <| gogTableTitle :: (List.map gogTableRow model.entries.left)
             , table[class <| "inlineTable"] <| steamTableTitle :: (List.map steamTableRow model.entries.right)
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
    Task.perform DataError ReceiveData (Http.get decodeResponse url)

decodeResponse : Json.Decoder Entries
decodeResponse =
  tuple2
    Entries
    (list <| object3 GogEntry ("id" := int) ("title" := string) ("gogId" := int))
    (list <| object3 SteamEntry ("id" := int) ("name" := string) ("steamId" := int))
