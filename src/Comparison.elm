module Comparison exposing (..)
import AllDict exposing (AllDict)
import Html exposing (Html, button, div, text, span, table, tr, th, td, select, option)
import Html.Attributes exposing(class, selected, value)
import Html.App as App
import Html.Events exposing (onClick, on, targetValue)
import Http
import Json.Decode as Json exposing (tuple3, int, string, list, object3, (:=))
import Task
import String
import Model exposing (..)

main =
    App.program { init = ( initialModel, refresh initialModel),
    view = view, update = update, subscriptions = \_ -> Sub.none }

-- MODEL
type PageSide = Left | Right

type alias NamedEntry = {internalId : Int, externalId : Int, name : String}

type alias ComparisonEntry = {left : NamedEntry, metricResult : Int, right : NamedEntry}

type alias Model = {comparisons : List ComparisonEntry, leftOn : GameOn, rightOn : GameOn, minimumMetric : Int, message : String}

initialModel = Model [] Gog Steam 3 ""

refresh model =
    getResponse "comparison/data" [("left", toString model.leftOn), ("right", toString model.rightOn), ("minimumMetric", toString model.minimumMetric)]

getOn side model =
    if side == Left then model.leftOn else model.rightOn

-- UPDATE

type Msg
  = ReceiveData (List ComparisonEntry)
  | DataError Http.Error
  | Refresh PageSide String
  | Increment
  | Decrement

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceiveData comparisons -> ({model | comparisons = comparisons} , Cmd.none)
    DataError err -> ({initialModel | message = toString err} , Cmd.none)
    Refresh side value ->
        let
            gameOn = if value == "Steam" then Steam else Gog
            newModel = if side == Left then {model | leftOn = gameOn} else {model | rightOn = gameOn}
        in
            ({ newModel | comparisons = []}, refresh newModel)
    Increment ->
        let
            newModel = {model | minimumMetric = model.minimumMetric + 1}
        in
            (newModel , refresh newModel)
    Decrement ->
        let
            newModel = {model | minimumMetric = model.minimumMetric - 1}
        in
            (newModel , refresh newModel)
-- VIEW

view : Model -> Html Msg
view model =
  div [] <|
    [ div [] (if String.isEmpty model.message then [] else [ text (toString model.message) ])
    , div [] [ table[class <| "inlineTable"] <| selectedSource Left model  :: selectedSource Right model :: title model  :: (List.map tableRow model.comparisons)
             ]
    ]

selectedSource side model =
    let
        gameOn = getOn side model
    in
        select [onSelect <| Refresh side] [option [selected (gameOn == Gog), value <| toString Gog][text <| toString Gog], option [selected (gameOn == Steam), value <| toString Steam][text <| toString Steam]]

tableRow e = tr [] [ td[][text e.left.name]
                   , td[][text <| toString e.metricResult]
                   , td[][text e.right.name] ]

title model =
    let
        tableTitle t1 t2 = tr [] [ th[][text t1]
                                 , th[] <| metricButtons model.minimumMetric
                                 , th[][text t2]]
        getTitle on = case on of
                          Gog -> "Gog Games"
                          Steam -> "Steam Games"
    in
        tableTitle (getTitle model.leftOn) (getTitle model.rightOn)


metricButtons value =
    [ button [ onClick Increment ] [ text "+" ]
        , div [] [ text <| "Maximum editing distance " ++ (toString (value - 1)) ]
        , button [ onClick Decrement ] [ text "-" ]
    ]

getResponse : String -> List ( String, String ) -> Cmd Msg
getResponse address params =
  let
    url = "http://localhost:9000/" ++ address ++ "?" ++ (joinParameters params)
  in
    Task.perform DataError ReceiveData (Http.get decodeResponse url)

joinParameters params =
    String.join "&&" (List.map (\((k, v)) -> k ++ "=" ++ v) params)

onSelect : (String -> Msg) -> Html.Attribute Msg
onSelect msg =
    on "change" (Json.map msg targetValue)

decodeResponse : Json.Decoder (List ComparisonEntry)
decodeResponse =
    list (object3 ComparisonEntry ("left" := namedEntryJson) ("metricResult" := int) ("left" := namedEntryJson))
namedEntryJson = object3 NamedEntry ("internalId" := int) ("externalId" := int) ("name" := string)
