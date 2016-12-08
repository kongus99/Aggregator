port module Comparison exposing (..)
import AllDict exposing (AllDict)
import Html exposing (Html, button, div, text, span, table, tr, th, td, select, option, input)
import Html.Attributes exposing(class, selected, value, type_, checked)
import Html.Events exposing (onClick, on, targetValue)
import Http
import Json.Decode as Json exposing (string, map3, field, decodeString)
import Task
import String
import Router exposing (..)
import Model exposing (..)

initProgram : String -> ( Model, Cmd Msg )
initProgram address =
    let
        parseInt value = String.toInt value |> Result.toMaybe |> Maybe.withDefault 0
        decodeAddress = map3 ComparisonParameters (field "left" <| Json.map gameOnFromString string) (field "right" <| Json.map gameOnFromString string) (field "minimumMetric" <| Json.map parseInt string)
        decodedParameters = Json.decodeString decodeAddress address |> Result.toMaybe |> Maybe.withDefault initialModel.parameters
    in
        ( {initialModel | parameters = decodedParameters}, refresh decodedParameters)

main = Html.programWithFlags { init = initProgram, view = view, update = update, subscriptions = \_ -> Sub.none }

-- PORTS
port elmAddressChange : String -> Cmd msg

-- MODEL
type PageSide = Left | Right

type alias Model = { comparisons : List ComparisonEntry, parameters : ComparisonParameters, message : String}

type alias ComparisonParameters = {leftOn : GameOn, rightOn : GameOn, minimumMetric : Int}

initialModel = Model [] (ComparisonParameters Gog Steam 3) ""

refresh parameters =
    getResponse [("left", toString parameters.leftOn), ("right", toString parameters.rightOn), ("minimumMetric", toString parameters.minimumMetric)]

gameOnFromString value = if value == "Steam" then Steam else Gog
-- UPDATE

type Msg
  = ReceiveData (List ComparisonEntry)
  | DataError Http.Error
  | RefreshData ComparisonParameters
  | Toggle Int Int
  | ToggleStored String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceiveData comparisons -> ({model | comparisons = comparisons} , Cmd.none)
    DataError err -> ({initialModel | message = toString err} , Cmd.none)
    RefreshData parameters -> ({ model | comparisons = [], parameters = parameters}, refresh parameters)
    Toggle leftId rightId ->
        let
            updateEntry e =
                if e.left.id == leftId && e.right.id == rightId
                then {e | matches = not e.matches}
                else e
            newComparisons = List.map updateEntry model.comparisons
        in
            ({model | comparisons = newComparisons}, postUpdate [("leftOn", toString model.parameters.leftOn), ("rightOn", toString model.parameters.rightOn), ("leftId", toString leftId), ("rightId", toString rightId)])
    ToggleStored mess ->
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
        refreshSide on =
            if side == Left then RefreshData {parameters | leftOn = gameOnFromString on} else RefreshData {parameters | rightOn = gameOnFromString on}
        gameOn = if side == Left then parameters.leftOn else parameters.rightOn
    in
        select [onSelect refreshSide] [option [selected (gameOn == Gog), value <| toString Gog][text <| toString Gog], option [selected (gameOn == Steam), value <| toString Steam][text <| toString Steam]]

tableRow model e =
            tr [] [ td[][text e.left.name]
                  , td[][text <| toString e.metricResult]
                  , td[][text e.right.name]
                  , td[][input[onClick <| Toggle e.left.id e.right.id ,type_ "checkbox", checked e.matches][]] ]

title model =
    let
        tableTitle t1 t2 = tr [] [ th[][text t1]
                                 , th[] <| metricButtons model.parameters
                                 , th[][text t2]
                                 , th[][text "Matches"] ]
        getTitle on = case on of
                          Gog -> "Gog Games"
                          Steam -> "Steam Games"
    in
        tableTitle (getTitle model.parameters.leftOn) (getTitle model.parameters.rightOn)


metricButtons parameters =
    let
        increment = RefreshData {parameters | minimumMetric = parameters.minimumMetric + 1}
        decrement = RefreshData {parameters | minimumMetric = parameters.minimumMetric - 1}
    in
        [ button [ onClick increment ] [ text "+" ]
            , div [] [ text <| "Editing distance less than " ++ (toString parameters.minimumMetric) ]
            , button [ onClick decrement ] [ text "-" ]
        ]

postUpdate params = Http.send (Router.resolveResponse ToggleStored DataError) (Router.toggleSelected params)

getResponse params =
    let
        (request, address) = Router.comparisonData params
    in
        Cmd.batch [Http.send (Router.resolveResponse ReceiveData DataError) request, elmAddressChange address]

onSelect : (String -> a) -> Html.Attribute a
onSelect msg =
    on "change" (Json.map msg targetValue)


