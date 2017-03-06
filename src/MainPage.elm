port module MainPage exposing (..)

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
        model =
            Erl.parse address |> initModel
    in
        ( model, refreshGames model )


main =
    Html.programWithFlags { init = initProgram, view = view, update = update, subscriptions = subscriptions }



-- PORTS


port elmAddressChange : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen (Router.refreshSocketUrl model.host) ServerRefreshRequest



-- MODEL


type alias Model =
    { sources : GameSources, message : Maybe String, userId : Int, filters : Filters, host : String, options : GameOptionsDialog.Model Msg }


initialModel sources userId host filters =
    Model WishList Nothing 1 filters "" (GameOptionsDialog.emptyModel DialogClose DialogMessage)


initModel : Erl.Url -> Model
initModel url =
    let
        userId =
            Erl.getQueryValuesForKey "userId" url |> List.head |> Maybe.andThen Parser.parseInt |> Maybe.withDefault 1

        sources =
            Erl.getQueryValuesForKey "sources" url |> List.head |> Maybe.andThen Parser.parseSources |> Maybe.withDefault WishList

        host =
            (url.host |> String.join ".") ++ ":" ++ toString url.port_
    in
        initialModel sources userId host (GameEntry.parseFilters url)


extractedParams : Model -> List ( String, String )
extractedParams model =
    List.append [ ( "sources", toString model.sources ), ( "userId", toString model.userId ) ] (serializeFilters model.filters)


adjustAddress : Model -> Cmd Msg
adjustAddress model =
    extractedParams model |> Router.mainPageUrl |> elmAddressChange


refreshGames : Model -> Cmd Msg
refreshGames model =
    extractedParams model |> Router.getUserGames |> Http.send (Router.resolveResponse ReceiveRefresh RefreshError)



-- UPDATE


type Msg
    = ChangeSources GameSources
    | ReceiveRefresh (List GameEntry)
    | RefreshError Http.Error
    | NameFilterChange String
    | LowPriceFilterChange String
    | HighPriceFilterChange String
    | GameOnFilterChange (Maybe GameOn)
    | DiscountedFilterChange Bool
    | ServerRefreshRequest String
    | DialogOpen (Maybe Int)
    | DialogData GameOptions
    | DialogClose
    | DialogMessage GameOptionsDialog.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSources s ->
            let
                newModel =
                    { model | filters = resetFilterLists model.filters, sources = s, message = Nothing }
            in
                ( newModel, refreshGames newModel )

        ReceiveRefresh entries ->
            ( { model | filters = updateFilterLists entries model.filters, message = Nothing }, adjustAddress model )

        RefreshError err ->
            ( { model | filters = resetFilterLists model.filters, message = Just <| toString err }, Cmd.none )

        NameFilterChange name ->
            let
                newModel =
                    { model | filters = updateNameFilter name model.filters, message = Nothing }
            in
                ( newModel, adjustAddress newModel )

        LowPriceFilterChange priceString ->
            let
                newModel =
                    { model | filters = updateLowFilter (Parser.parseFloat priceString) model.filters, message = Nothing }
            in
                ( newModel, adjustAddress newModel )

        HighPriceFilterChange priceString ->
            let
                newModel =
                    { model | filters = updateHighFilter (Parser.parseFloat priceString) model.filters, message = Nothing }
            in
                ( newModel, adjustAddress newModel )

        GameOnFilterChange gameOn ->
            let
                newModel =
                    { model | filters = updateGameOnFilter gameOn model.filters, message = Nothing }
            in
                ( newModel, adjustAddress newModel )

        DiscountedFilterChange isDiscounted ->
            let
                newModel =
                    { model | filters = toggleDiscountedFilter isDiscounted model.filters, message = Nothing }
            in
                ( newModel, adjustAddress newModel )

        ServerRefreshRequest msg ->
            ( { model | filters = resetFilterLists model.filters, message = Nothing }, refreshGames model )

        DialogOpen steamId ->
            ( model, GameOptionsDialog.fetch model.userId steamId DialogData RefreshError )

        DialogData options ->
            ( { model | options = GameOptionsDialog.model DialogClose DialogMessage options }, Cmd.none )

        DialogClose ->
            ( { model | options = GameOptionsDialog.emptyModel DialogClose DialogMessage }, Cmd.none )

        DialogMessage msg ->
            let
                updated =
                    GameOptionsDialog.update model.userId msg model.options
            in
                ( { model | options = Tuple.first updated }, Tuple.second updated )



-- VIEW


view : Model -> Html Msg
view model =
    table [ class "table table-striped table-bordered" ]
        [ thead [] [ gameTableTitle ]
        , th [ class "form-inline" ]
            [ div [ class "form-group" ]
                [ discountedInput model.filters.isDiscounted
                , input [ placeholder "Name", class "form-control", type_ "text", onInput NameFilterChange, value model.filters.name ] []
                , sourcesSelect model.sources
                , gameOnSelect model.filters.gameOn
                ]
            ]
        , th [ class "form-inline" ]
            [ div [ class "form-group" ]
                [ input [ placeholder "Lowest price", class "form-control", type_ "text", onInput LowPriceFilterChange, value <| Maybe.withDefault "" <| Maybe.map toString <| Tuple.first model.filters.prices ] []
                , input [ placeholder "Highest price", class "form-control", type_ "text", onInput HighPriceFilterChange, value <| Maybe.withDefault "" <| Maybe.map toString <| Tuple.second model.filters.prices ] []
                ]
            ]
        , th [] []
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


sourcesSelect sources =
    let
        change s =
            Parser.parseSources s |> Maybe.withDefault Both |> ChangeSources
    in
        select [ class "form-control", onSelect change ]
            [ option [ selected (sources == Owned), value <| toString Owned ] [ text <| toString Owned ]
            , option [ selected (sources == WishList), value <| toString WishList ] [ text <| toString WishList ]
            , option [ selected (sources == Both), value <| toString Both ] [ text <| toString Both ]
            ]


gameOnSelect maybeGameOn =
    let
        change s =
            Parser.parseGameOn s |> GameOnFilterChange
    in
        select [ class "form-control", onSelect change ]
            [ option [ selected (maybeGameOn == Nothing), value "" ] [ text "" ]
            , option [ selected (maybeGameOn == Just Steam), value <| toString Steam ] [ text <| toString Steam ]
            , option [ selected (maybeGameOn == Just Gog), value <| toString Gog ] [ text <| toString Gog ]
            ]


discountedInput isDiscounted =
    div [ class "checkbox" ]
        [ label [] [ input [ type_ "checkbox", name "Discounted", checked isDiscounted, onCheck DiscountedFilterChange ] [], text "Discounted" ]
        ]


gamesOn list =
    List.map
        (\e ->
            if e == "Gog" then
                Gog
            else
                Steam
        )
        list


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


onSelect : (String -> a) -> Html.Attribute a
onSelect msg =
    on "change" (Json.map msg targetValue)
