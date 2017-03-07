port module MainPage exposing (..)

import Filters
import GameOptionsDialog
import Parser
import Html exposing (Html, button, br, input, div, text, span, table, tr, th, td, select, option, a, label, thead, tbody, p, h2, h3)
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
import Dialog
import List.Extra as Lists


initProgram : String -> ( Model, Cmd Msg )
initProgram address =
    let
        url =
            Erl.parse address

        host =
            (url.host |> String.join ".") ++ ":" ++ toString url.port_

        ( filters, cmd ) =
            Filters.parse url |> Filters.refresh ""
    in
        ( initialModel host filters, Cmd.map FiltersMessage cmd )


main =
    Html.programWithFlags { init = initProgram, view = view, update = update, subscriptions = subscriptions }



-- PORTS


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen (Router.refreshSocketUrl model.host) ServerRefreshRequest



-- MODEL


type alias Model =
    { sources : GameSources, message : Maybe String, userId : Int, filters : Filters.Model, host : String, options : GameOptionsDialog.Model Msg }


initialModel host filters =
    Model WishList Nothing 1 filters "" (GameOptionsDialog.emptyModel DialogClose DialogMessage)



-- UPDATE


type Msg
    = ServerRefreshRequest String
    | DialogOpen (Maybe Int)
    | DialogData GameOptions
    | DialogClose
    | DialogMessage GameOptionsDialog.Msg
    | FiltersMessage Filters.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ServerRefreshRequest s ->
            let
                ( newFilters, cmd ) =
                    Filters.refresh s model.filters
            in
                ( { model | filters = newFilters, message = newFilters.err |> Maybe.map toString }, Cmd.map FiltersMessage cmd )

        DialogOpen steamId ->
            ( model, Cmd.none )

        --            ( model, GameOptionsDialog.fetch model.userId steamId DialogData RefreshError )
        DialogData options ->
            ( model, Cmd.none )

        --            ( { model | options = GameOptionsDialog.model DialogClose DialogMessage options }, Cmd.none )
        DialogClose ->
            ( model, Cmd.none )

        --            ( { model | options = GameOptionsDialog.emptyModel DialogClose DialogMessage }, Cmd.none )
        DialogMessage msg ->
            ( model, Cmd.none )

        --            let
        --                ( options, cmd ) =
        --                    GameOptionsDialog.update model.userId msg model.options
        --            in
        --                ( { model | options = options }, cmd )
        FiltersMessage msg ->
            let
                ( newFilters, cmd ) =
                    Filters.update msg model.filters
            in
                ( { model | filters = newFilters, message = newFilters.err |> Maybe.map toString }, Cmd.map FiltersMessage cmd )



-- VIEW


view : Model -> Html Msg
view model =
    table [ class "table table-striped table-bordered" ]
        [ thead [] [ gameTableTitle ]
        , Filters.view model.filters |> Html.map FiltersMessage
        , tbody [] (List.map gameTableRow model.filters.result)
        , GameOptionsDialog.view model.options
        ]


messageText model =
    Maybe.map (\t -> div [] [ text t ]) model.message |> Maybe.withDefault (div [] [])


gameTableTitle =
    tr []
        [ th [] [ text "Game - ", span [ class "cell_Steam" ] [ text " Steam" ], span [ class "cell_Gog" ] [ text " Gog" ], span [ class "cell_Both" ] [ text " Both" ] ]
        , th [] [ text "Price(PLN)" ]
        , th [] [ text "Additional prices(PLN)" ]
        ]


gameTableRow e =
    tr []
        [ th [] [ a [ href <| getLink e, class <| toStyle e ] [ text <| getName e ], gameOptionsButton e ]
        , td [ class "text-right" ] [ text <| pricesToString (getPrice e) ]
        , td [] (additionalPrices e.prices)
        ]


gameOptionsButton entry =
    let
        dialogButton e =
            button [ onClick <| DialogOpen <| getSteamId e, class "glyphicon glyphicon-cog btn btn-default", style [ ( "float", "right" ) ] ] []
    in
        List.head entry.steam |> Maybe.map (\_ -> dialogButton entry) |> Maybe.withDefault (div [] [])


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
    let
        onGog =
            List.length gameEntry.gog > 0

        onSteam =
            List.length gameEntry.steam > 0
    in
        if onGog && onSteam then
            "cell_Both"
        else if onGog then
            "cell_Gog"
        else
            "cell_Steam"
