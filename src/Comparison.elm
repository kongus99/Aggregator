port module Comparison exposing (..)
import AllDict exposing (AllDict)
import Html exposing (Html, button, div, text, span, table, tr, th, td, select, option, input)
import Html.Attributes exposing(class, selected, value, type', checked)
import Html.App as App
import Html.Events exposing (onClick, on, targetValue)
import Http
import Json.Decode as Json exposing (object4, int, string, list, object3, (:=), bool, decodeString)
import Task
import String
import Model exposing (..)

main =
    App.program { init = ( initialModel, refresh initialModel.baseUrl initialModel.parameters),
    view = view, update = update, subscriptions = subscriptions }

-- PORTS
port elmAddressChange : String -> Cmd msg

port pageLoadAddress : (String ->  msg) -> Sub msg

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
     pageLoadAddress LoadAddress

-- MODEL
type PageSide = Left | Right

type alias NamedEntry = {internalId : Int, externalId : Int, name : String}

type alias ComparisonEntry = {left : NamedEntry, metricResult : Int, right : NamedEntry, matches : Bool}

type alias Model = {baseUrl : String, comparisons : List ComparisonEntry, parameters : ComparisonParameters, message : String}

type alias ComparisonParameters = {leftOn : GameOn, rightOn : GameOn, minimumMetric : Int}

initialModel = Model "http://localhost:9000/comparison" [] (ComparisonParameters Gog Steam 3) ""

refresh url parameters =
    getResponse url "/data" [("left", toString parameters.leftOn), ("right", toString parameters.rightOn), ("minimumMetric", toString parameters.minimumMetric)]

gameOnFromString value = if value == "Steam" then Steam else Gog
-- UPDATE

type Msg
  = ReceiveData (List ComparisonEntry)
  | DataError Http.Error
  | Refresh PageSide String
  | RefreshData ComparisonParameters
  | Increment
  | Decrement
  | Toggle GameOn GameOn Int Int
  | ToggleStored String
  | LoadAddress String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceiveData comparisons -> ({model | comparisons = comparisons} , Cmd.none)
    DataError err -> ({initialModel | message = toString err} , Cmd.none)
    RefreshData parameters ->
        ({ model | comparisons = [], parameters = parameters}, refresh model.baseUrl parameters)
    Refresh side value ->
        let
            currentParameters = model.parameters
            newParameters = if side == Left then {currentParameters | leftOn = gameOnFromString value} else {currentParameters | rightOn = gameOnFromString value}
        in
            ({ model | comparisons = [], parameters = newParameters}, refresh model.baseUrl newParameters)
    Increment ->
        let
            currentParameters = model.parameters
            newParameters = {currentParameters | minimumMetric = currentParameters.minimumMetric + 1}
        in
            ({ model | comparisons = [], parameters = newParameters} , refresh model.baseUrl newParameters)
    Decrement ->
        let
            currentParameters = model.parameters
            newParameters = {currentParameters | minimumMetric = currentParameters.minimumMetric - 1}
        in
            ({ model | comparisons = [], parameters = newParameters} , refresh model.baseUrl newParameters)
    Toggle leftOn rightOn leftId rightId ->
        let
            updateEntry e =
                if e.left.externalId == leftId && e.right.externalId == rightId
                then {e | matches = not e.matches}
                else e
            newComparisons = List.map updateEntry model.comparisons
        in
            ({model | comparisons = newComparisons}, postUpdate model "/toggleMatch" [("leftOn", toString leftOn), ("rightOn", toString rightOn), ("leftId", toString leftId), ("rightId", toString rightId)])
    ToggleStored mess ->
        (model , Cmd.none)
    LoadAddress address ->
        let
            parseInt value = String.toInt value |> Result.toMaybe |> Maybe.withDefault 0
            decodeAddress = object3 ComparisonParameters ("left" := Json.map gameOnFromString string) ("right" := Json.map gameOnFromString string) ("minimumMetric" := Json.map parseInt string)
            y = Json.decodeString decodeAddress address
            x = Debug.log "XXX" address
            z = Debug.log "YYY" y
        in
            (model , Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div [] <|
    [ div [] (if String.isEmpty model.message then [] else [ text (toString model.message) ])
    , div [] [ table[class <| "inlineTable"] <| selectedSource Left model.parameters  :: selectedSource Right model.parameters :: title model  :: (List.map (tableRow model) model.comparisons)
             ]
    ]

selectedSource side parameters =
    let
        gameOn = if side == Left then parameters.leftOn else parameters.rightOn
    in
        select [onSelect <| Refresh side] [option [selected (gameOn == Gog), value <| toString Gog][text <| toString Gog], option [selected (gameOn == Steam), value <| toString Steam][text <| toString Steam]]

tableRow model e = tr [] [ td[][text e.left.name]
                   , td[][text <| toString e.metricResult]
                   , td[][text e.right.name]
                   , td[][input[onClick <| Toggle model.parameters.leftOn model.parameters.rightOn e.left.externalId e.right.externalId ,type' "checkbox", checked e.matches][]] ]

title model =
    let
        tableTitle t1 t2 = tr [] [ th[][text t1]
                                 , th[] <| metricButtons model.parameters.minimumMetric
                                 , th[][text t2]
                                 , th[][text "Matches"] ]
        getTitle on = case on of
                          Gog -> "Gog Games"
                          Steam -> "Steam Games"
    in
        tableTitle (getTitle model.parameters.leftOn) (getTitle model.parameters.rightOn)


metricButtons value =
    [ button [ onClick Increment ] [ text "+" ]
        , div [] [ text <| "Maximum editing distance " ++ (toString (value - 1)) ]
        , button [ onClick Decrement ] [ text "-" ]
    ]

postUpdate model address params =
    let
        url = model.baseUrl ++ address ++ "?" ++ (joinParameters params)
      in
        Task.perform DataError ToggleStored (Http.post string url Http.empty)

getResponse : String -> String -> List ( String, String ) -> Cmd Msg
getResponse prefix suffix params =
  let
    dataUrl = prefix ++ suffix ++ "?" ++ (joinParameters params)
    pageUrl = prefix ++ "?" ++ (joinParameters params)
  in
    Cmd.batch [Task.perform DataError ReceiveData (Http.get decodeResponse dataUrl), elmAddressChange pageUrl]

joinParameters params =
    String.join "&&" (List.map (\((k, v)) -> k ++ "=" ++ v) params)

onSelect : (String -> Msg) -> Html.Attribute Msg
onSelect msg =
    on "change" (Json.map msg targetValue)

decodeResponse : Json.Decoder (List ComparisonEntry)
decodeResponse =
    list (object4 ComparisonEntry ("left" := namedEntryJson) ("metricResult" := int) ("right" := namedEntryJson) ("matches" := bool))
namedEntryJson = object3 NamedEntry ("internalId" := int) ("externalId" := int) ("name" := string)
