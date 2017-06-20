port module Comparison exposing (..)

import AllDict exposing (AllDict)
import Html exposing (Html, button, div, input, option, select, span, table, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, contenteditable, for, readonly, selected, type_, value)
import Html.Events exposing (onClick, on, targetValue)
import HtmlHelpers exposing (onMenuItemCheck, onSelect)
import Http
import Json.Decode as Json exposing (string, map3, field, decodeString)
import Task
import Bootstrap.CDN as CDN
import Bootstrap.Table as Table
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import String
import Router exposing (..)
import Model exposing (..)
import Parser


initProgram : String -> ( Model, Cmd Msg )
initProgram address =
    let
        parseInt value =
            String.toInt value |> Result.toMaybe |> Maybe.withDefault 0

        decodeAddress =
            map3 ComparisonParameters (field "left" <| Json.map gameOnFromString string) (field "right" <| Json.map gameOnFromString string) (field "minimumMetric" <| Json.map parseInt string)

        decodedParameters =
            Json.decodeString decodeAddress address |> Result.toMaybe |> Maybe.withDefault initialModel.parameters
    in
        ( { initialModel | parameters = decodedParameters }, refresh decodedParameters )


main =
    Html.programWithFlags { init = initProgram, view = view, update = update, subscriptions = \_ -> Sub.none }



-- PORTS


port elmAddressChange : String -> Cmd msg



-- MODEL


type PageSide
    = Left
    | Right


type alias Model =
    { comparisons : List ComparisonEntry, parameters : ComparisonParameters, message : String }


type alias ComparisonParameters =
    { leftOn : GameOn, rightOn : GameOn, minimumMetric : Int }


initialModel =
    Model [] (ComparisonParameters Gog Steam 3) ""


refresh parameters =
    getResponse [ ( "left", toString parameters.leftOn ), ( "right", toString parameters.rightOn ), ( "minimumMetric", toString parameters.minimumMetric ) ]


gameOnFromString value =
    if value == "Steam" then
        Steam
    else
        Gog



-- UPDATE


type Msg
    = ReceiveData (List ComparisonEntry)
    | DataError Http.Error
    | RefreshData ComparisonParameters
    | Toggle Int Int Bool
    | ToggleStored String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveData comparisons ->
            ( { model | comparisons = comparisons }, Cmd.none )

        DataError err ->
            ( { initialModel | message = toString err }, Cmd.none )

        RefreshData parameters ->
            ( { model | comparisons = [], parameters = parameters }, refresh parameters )

        Toggle leftId rightId toggle ->
            let
                updateEntry e =
                    if e.left.id == leftId && e.right.id == rightId then
                        { e | matches = toggle }
                    else
                        e

                newComparisons =
                    List.map updateEntry model.comparisons
            in
                ( { model | comparisons = newComparisons }
                , postUpdate
                    [ ( "leftOn", toString model.parameters.leftOn )
                    , ( "rightOn", toString model.parameters.rightOn )
                    , ( "leftId", toString leftId )
                    , ( "rightId", toString rightId )
                    ]
                )

        ToggleStored mess ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ CDN.stylesheet
        , div []
            (if String.isEmpty model.message then
                []
             else
                [ text (toString model.message) ]
            )
        , Table.table
            { options = [ Table.striped, Table.bordered ]
            , thead = title model
            , tbody = tableRows model.comparisons
            }
        ]


selectedSource side parameters =
    let
        refreshSide on =
            if side == Left then
                RefreshData { parameters | leftOn = on }
            else
                RefreshData { parameters | rightOn = on }

        gameOn =
            if side == Left then
                parameters.leftOn
            else
                parameters.rightOn
    in
        ButtonGroup.radioButtonGroup [ ButtonGroup.small ]
            [ ButtonGroup.radioButton (gameOn == Gog) [ Button.secondary, Button.onClick <| refreshSide Gog ] [ text <| toString Gog ]
            , ButtonGroup.radioButton (gameOn == Steam) [ Button.secondary, Button.onClick <| refreshSide Steam ] [ text <| toString Steam ]
            ]


tableRows comparisonList =
    Table.tbody [] <| List.map tableRow comparisonList


tableRow comparison =
    Table.tr []
        [ Table.td [] [ text <| toString comparison.metricResult ]
        , Table.td [] [ text comparison.left.name ]
        , Table.td [] [ text comparison.right.name ]
        , Table.td []
            [ ButtonGroup.checkboxButtonGroup [ ButtonGroup.small ]
                [ ButtonGroup.checkboxButton comparison.matches
                    [ Button.attrs
                        [ if comparison.matches then
                            class "fa fa-check"
                          else
                            class "fa fa-times"
                        , onMenuItemCheck <| Toggle comparison.left.id comparison.right.id
                        ]
                    , Button.secondary
                    ]
                    []
                ]
            ]
        ]


title model =
    Table.simpleThead
        [ Table.th [] [ metricButtons model.parameters ]
        , Table.th [ Table.cellAttr <| class "center" ] [ selectedSource Left model.parameters ]
        , Table.th [ Table.cellAttr <| class "center" ] [ selectedSource Right model.parameters ]
        , Table.th [ Table.cellAttr <| class "center" ] [ text "Matches" ]
        ]


metricButtons parameters =
    Form.formInline []
        [ Form.group []
            [ Form.label [ for "distanceSpinner" ] [ text "Editing distance less than " ]
            , Input.number
                [ Input.id "distanceSpinner"
                , Input.value <| toString parameters.minimumMetric
                , Input.onInput (\v -> RefreshData { parameters | minimumMetric = Parser.parseInt v |> Maybe.withDefault 1 })
                ]
            ]
        ]


postUpdate params =
    params
        |> routes.comparison.toggleSelected
        |> .request
        |> Http.send (Router.resolveResponse ToggleStored DataError)


getResponse params =
    let
        request =
            params |> routes.comparison.comparisonData |> .request

        url =
            params |> routes.comparison.page |> .url
    in
        Cmd.batch [ Http.send (Router.resolveResponse ReceiveData DataError) request, elmAddressChange url ]
