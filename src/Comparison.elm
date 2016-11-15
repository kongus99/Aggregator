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
    App.program { init = ( initialModel, getResponse "comparison/data" [("left", initialModel.leftOn), ("right", initialModel.rightOn)]),
    view = view, update = update, subscriptions = \_ -> Sub.none }

-- MODEL
type alias NamedEntry = {internalId : Int, externalId : Int, name : String}

type alias Entries = {left : List NamedEntry, right : List NamedEntry}

type alias Model = {entries : Entries, leftOn : GameOn, rightOn : GameOn,message : String}

initialModel = {entries = Entries [] [], leftOn = Gog, rightOn = Steam, message = ""}

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
    , div [] [ table[class <| "inlineTable"] <| title model.leftOn tableTitle :: (List.map tableRow model.entries.left)
             , table[class <| "inlineTable"] <| title model.rightOn tableTitle :: (List.map tableRow model.entries.right)
             ]
    ]

tableTitle title = tr [] [ th[][text title] ]

tableRow e = tr [] [ td[][text e.name] ]

title on fn =
    case on of
        Gog -> fn "Gog Game"
        Steam -> fn "Steam Game"

getResponse address params =
  let
    url = "http://localhost:9000/" ++ address ++ "?" ++ (joinParameters params)
  in
    Task.perform DataError ReceiveData (Http.get decodeResponse url)

joinParameters params =
    String.join "&&" (List.map (\((k, v)) -> k ++ "=" ++ (toString v)) params)

decodeResponse : Json.Decoder Entries
decodeResponse =
  tuple2
    Entries
    (list <| object3 NamedEntry ("internalId" := int) ("externalId" := int) ("name" := string))
    (list <| object3 NamedEntry ("internalId" := int) ("externalId" := int) ("name" := string))
