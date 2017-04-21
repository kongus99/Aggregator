port module MainPage exposing (..)

import Filters
import GameOptionsDialog
import Parser
import Html exposing (Html, button, br, input, div, text, span, tr, th, td, select, option, a, label, thead, tbody, p, h2, h3)
import Html.Attributes exposing (checked, class, href, name, placeholder, selected, style, type_, value)
import Html.Events exposing (onClick, on, targetValue, onInput, onCheck)
import Json.Decode as Json
import GameEntry exposing (..)
import Http
import Task
import Erl
import Model exposing (..)
import Router exposing (..)
import WebSocket
import List.Extra as Lists
import Bootstrap.CDN as CDN
import Bootstrap.Table as Table
import Bootstrap.Button as Button
import Bootstrap.Modal as Modal


initProgram : String -> ( Model, Cmd Msg )
initProgram address =
    let
        url =
            Erl.parse address

        host =
            (url.host |> String.join ".") ++ ":" ++ toString url.port_

        protocol =
            url.protocol |> Parser.parseProtocol

        ( filters, cmd ) =
            Filters.initialize url
    in
        ( initialModel protocol host filters, Cmd.map FiltersMessage cmd )


main =
    Html.programWithFlags { init = initProgram, view = view, update = update, subscriptions = subscriptions }



-- PORTS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen (Router.refreshSocketUrl model.protocol model.host model.filters.userId) ServerRefreshRequest
        , Filters.subscriptions model.filters |> Sub.map FiltersMessage
        ]


port elmAddressChange : String -> Cmd msg



-- MODEL


type alias Model =
    { sources : GameSources, message : Maybe String, filters : Filters.Model, host : String, protocol : Protocol, options : GameOptionsDialog.Model Msg }


initialModel protocol host filters =
    Model WishList Nothing filters host protocol (GameOptionsDialog.emptyModel 0 0 DialogMessage GeneralError)



-- UPDATE


type Msg
    = ServerRefreshRequest String
    | DialogOpen (Maybe Int)
    | DialogClose Modal.State
    | GeneralError Http.Error
    | DialogMessage GameOptionsDialog.Msg
    | FiltersMessage Filters.Msg
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
                    Filters.serialize model.filters |> routes.main.page |> .url |> elmAddressChange
            in
                newModel ! [ Cmd.map FiltersMessage cmd, adjustAddress newModel ]

        Ack msg ->
            model ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ CDN.stylesheet
        , Filters.view model.filters |> Html.map FiltersMessage
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
        , Table.td [ Table.cellAttr (class "text-right") ] [ e.prices |> Maybe.map serializeValue |> Maybe.withDefault "" |> text ]
        , Table.td [] (additionalPrices e.additionalPrices)
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


additionalPrices priceEntries =
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
