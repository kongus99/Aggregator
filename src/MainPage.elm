port module MainPage exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Modal as Modal
import Bootstrap.Table as Table
import Filters
import GameEntry exposing (..)
import GameOptionsDialog
import Html exposing (Html, a, br, button, div, h2, h3, input, label, option, p, select, span, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, href, name, placeholder, selected, style, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput, targetValue)
import Http
import Json.Decode as Json
import List.Extra as Lists
import Model exposing (..)
import Navigation
import Parser
import Price exposing (roundToString)
import Router exposing (..)
import Task
import WebSocket


initProgram : Navigation.Location -> ( Model, Cmd Msg )
initProgram location =
    let
        ( filters, cmd ) =
            Filters.initialize location
    in
    ( initialModel location filters, Cmd.map FiltersMessage cmd )


main =
    Navigation.program ChangeLocation { init = initProgram, view = view, update = update, subscriptions = subscriptions }



-- PORTS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen (Router.refreshSocketUrl model.location model.filters.userId) ServerRefreshRequest
        , Filters.subscriptions model.filters |> Sub.map FiltersMessage
        ]



-- MODEL


type alias Model =
    { sources : GameSources, message : Maybe String, filters : Filters.Model, location : Navigation.Location, options : GameOptionsDialog.Model Msg }


initialModel location filters =
    Model WishList Nothing filters location (GameOptionsDialog.emptyModel 0 0 DialogMessage GeneralError)



-- UPDATE


type Msg
    = ServerRefreshRequest String
    | DialogOpen (Maybe Int)
    | DialogClose Modal.State
    | GeneralError Http.Error
    | DialogMessage GameOptionsDialog.Msg
    | FiltersMessage Filters.Msg
    | ChangeLocation Navigation.Location
    | Ack String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ServerRefreshRequest s ->
            let
                newValues =
                    Router.decodeWebSocketResult s

                newFilters =
                    Filters.replace newValues model.filters
            in
            ( { model | filters = newFilters, message = newFilters.err |> Maybe.map toString }, Cmd.none )

        DialogOpen steamId ->
            let
                options =
                    GameOptionsDialog.emptyModel model.filters.userId (Maybe.withDefault 0 steamId) DialogMessage GeneralError
            in
            { model | options = options } ! [ GameOptionsDialog.open options ]

        DialogClose state ->
            let
                options =
                    GameOptionsDialog.close model.options
            in
            { model | options = options } ! [ GameOptionsDialog.refresh Ack options ]

        GeneralError err ->
            ( { model | message = toString err |> Just }, Cmd.none )

        DialogMessage msg ->
            let
                ( options, cmd ) =
                    GameOptionsDialog.update msg model.options
            in
            ( { model | options = options }, Cmd.map DialogMessage cmd )

        FiltersMessage msg ->
            let
                ( newFilters, cmd ) =
                    Filters.update msg model.filters

                newModel =
                    { model | filters = newFilters, message = newFilters.err |> Maybe.map toString }

                adjustAddress model =
                    Filters.serialize model.filters |> routes.main.page |> .url |> Navigation.newUrl
            in
            newModel ! [ Cmd.map FiltersMessage cmd, adjustAddress newModel ]

        ChangeLocation loc ->
            model ! []

        Ack msg ->
            model ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Filters.view model.filters |> Html.map FiltersMessage
        , Table.table
            { options = [ Table.striped, Table.bordered ]
            , thead = gameTableTitle
            , tbody = gameTableRows model.filters.result
            }
        , GameOptionsDialog.view DialogClose model.options
        ]


gameTableTitle =
    Table.simpleThead
        [ Table.th [] [ text "Game - ", span [ class "cell_Steam" ] [ text " Steam" ], span [ class "cell_Gog" ] [ text " Gog" ], span [ class "cell_Both" ] [ text " Both" ] ]
        , Table.th [] [ text "Genres" ]
        , Table.th [] [ text "Tags" ]
        , Table.th [] [ text "Price(PLN)" ]
        , Table.th [] [ text "Additional prices(PLN)" ]
        ]


gameTableRows list =
    Table.tbody [] <| List.map gameTableRow list


gameTableRow e =
    Table.tr []
        [ Table.th [] [ a [ href e.link, class <| toStyle e ] [ text e.name ], gameOptionsButton e ]
        , Table.td [ Table.cellAttr (class "text-left") ] [ text <| serializeValue e.genres ]
        , Table.td [ Table.cellAttr (class "text-center") ] [ text <| serializeValue e.tags ]
        , Table.td [ Table.cellAttr (class "text-right") ] [ e.price |> Maybe.map serializeValue |> Maybe.withDefault "" |> text ]
        , Table.td [] (alternatePrices e.alternatePrices)
        ]


gameOptionsButton entry =
    let
        dialogButton e =
            Button.linkButton
                [ Button.onClick <| DialogOpen <| e.steamId
                , Button.outlineSecondary
                , Button.small
                , Button.attrs [ class "fa fa-cog align-top float-right" ]
                ]
                []
    in
    entry.steamId |> Maybe.map (\_ -> dialogButton entry) |> Maybe.withDefault (div [] [])


alternatePrices priceEntries =
    let
        price e =
            div []
                [ div [] [ text e.name ]
                , div [] [ a [ href e.link ] [ text e.host ] ]
                , div [] [ text <| roundToString 2 e.price ]
                ]
    in
    List.map price priceEntries


toStyle gameEntry =
    if gameEntry.gameOn == Nothing then
        "cell_Both"
    else if gameEntry.gameOn == Just Gog then
        "cell_Gog"
    else
        "cell_Steam"
