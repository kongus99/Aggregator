module Filters exposing (Model, parse, Msg, update, refresh, view, serialize)

import Html exposing (Html, button, div, input, label, option, select, text, th)
import Html.Attributes exposing (checked, class, name, placeholder, selected, style, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput)
import HtmlHelpers exposing (onSelect)
import Http
import Model exposing (..)
import GameEntry exposing (GameEntry, getPrice, getName)
import Erl
import Parser
import Router
import WebSocket


-- MODEL


type alias Model =
    { userId : Int, sources : GameSources, isDiscounted : Bool, gameOn : Maybe GameOn, name : String, prices : ( Maybe Float, Maybe Float ), original : List GameEntry, result : List GameEntry, err : Maybe Http.Error }


replace : List GameEntry -> Model -> Model
replace list model =
    apply { model | original = list }


parse : Erl.Url -> Model
parse url =
    let
        userId =
            Erl.getQueryValuesForKey "userId" url |> List.head |> Maybe.andThen Parser.parseInt |> Maybe.withDefault 1

        sources =
            Erl.getQueryValuesForKey "sources" url |> List.head |> Maybe.andThen Parser.parseSources |> Maybe.withDefault WishList

        discounted =
            Erl.getQueryValuesForKey "discounted" url |> List.head |> Maybe.andThen Parser.parseBool |> Maybe.withDefault False

        gameOn =
            Erl.getQueryValuesForKey "gameOn" url |> List.head |> Maybe.andThen Parser.parseGameOn

        name =
            Erl.getQueryValuesForKey "name" url |> List.head |> Maybe.withDefault ""

        lowPrice =
            Erl.getQueryValuesForKey "lowPrice" url |> List.head |> Maybe.andThen Parser.parseFloat

        highPrice =
            Erl.getQueryValuesForKey "highPrice" url |> List.head |> Maybe.andThen Parser.parseFloat
    in
        Model userId sources discounted gameOn name ( lowPrice, highPrice ) [] [] Nothing


serialize : Model -> List ( String, String )
serialize model =
    [ ( "sources", toString model.sources |> Just )
    , ( "userId", toString model.userId |> Just )
    , ( "discounted", toString model.isDiscounted |> Just )
    , ( "gameOn", Maybe.map toString model.gameOn )
    , ( "name", model.name |> Just )
    , ( "lowPrice", Tuple.first model.prices |> Maybe.map toString )
    , ( "highPrice", Tuple.second model.prices |> Maybe.map toString )
    ]
        |> List.filter (\( l, r ) -> r /= Nothing)
        |> List.map (\( l, r ) -> ( l, Maybe.withDefault "" r ))


apply : Model -> Model
apply model =
    let
        result =
            applyDiscountedFilter model.isDiscounted model.original
                |> applyGameOnFilter model.gameOn
                |> applyNameFilter model.name
                |> applyPriceFilter model.prices
    in
        { model | result = result, err = Nothing }


applyDiscountedFilter : Bool -> List GameEntry -> List GameEntry
applyDiscountedFilter isDiscounted entries =
    let
        filterDiscounted e =
            Maybe.map (\p -> not (Tuple.second p == Nothing)) (getPrice e) |> Maybe.withDefault False
    in
        if isDiscounted then
            List.filter filterDiscounted entries
        else
            entries


applyGameOnFilter : Maybe GameOn -> List GameEntry -> List GameEntry
applyGameOnFilter gameOn entries =
    let
        isOn on entry =
            if on == Steam && List.isEmpty entry.steam || on == Gog && List.isEmpty entry.gog then
                False
            else
                True
    in
        Maybe.map (\g -> List.filter (isOn g) entries) gameOn |> Maybe.withDefault entries


applyNameFilter : String -> List GameEntry -> List GameEntry
applyNameFilter name entries =
    if String.isEmpty name then
        entries
    else
        List.filter (\e -> getName e |> String.toLower |> String.contains (String.toLower name)) entries


applyPriceFilter : ( Maybe Float, Maybe Float ) -> List GameEntry -> List GameEntry
applyPriceFilter ( lowPrice, highPrice ) entries =
    let
        filterByLow lowPrice entry =
            getPrice entry |> discountedIfAvailable |> Maybe.map (\e -> e >= lowPrice) |> Maybe.withDefault False

        filterByHigh highPrice entry =
            getPrice entry |> discountedIfAvailable |> Maybe.map (\e -> e <= highPrice) |> Maybe.withDefault False

        lowFiltered =
            Maybe.map (\p -> List.filter (filterByLow p) entries) lowPrice |> Maybe.withDefault entries
    in
        Maybe.map (\p -> List.filter (filterByHigh p) lowFiltered) highPrice |> Maybe.withDefault lowFiltered


discountedIfAvailable : Maybe ( Maybe Float, Maybe Float ) -> Maybe Float
discountedIfAvailable prices =
    let
        selectFromPair ( f, s ) =
            if s == Nothing then
                f
            else
                s
    in
        Maybe.andThen selectFromPair prices



-- UPDATE


type Msg
    = Clear
    | ChangeName String
    | ChangeLow String
    | ChangeHigh String
    | ChangeGameOn String
    | ChangeDiscounted Bool
    | ChangeSources String
    | ReceiveEntries (List GameEntry)
    | ReceiveError Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clear ->
            apply (Model model.userId model.sources False Nothing "" ( Nothing, Nothing ) model.original [] Nothing) ! []

        ChangeName name ->
            apply { model | name = name } ! []

        ChangeLow low ->
            apply { model | prices = ( Parser.parseFloat low, Tuple.second model.prices ) } ! []

        ChangeHigh high ->
            apply { model | prices = ( Tuple.first model.prices, Parser.parseFloat high ) } ! []

        ChangeGameOn on ->
            apply { model | gameOn = (Parser.parseGameOn on) } ! []

        ChangeDiscounted isDiscounted ->
            apply { model | isDiscounted = isDiscounted } ! []

        ChangeSources s ->
            let
                newModel =
                    apply { model | result = [], original = [], sources = (Parser.parseSources s |> Maybe.withDefault Both) }
            in
                newModel ! [ sendRefresh newModel ]

        ReceiveEntries entries ->
            replace entries model ! []

        ReceiveError err ->
            { model | err = Just err } ! []


refresh : String -> Model -> ( Model, Cmd Msg )
refresh s model =
    ( model, sendRefresh model )


sendRefresh : Model -> Cmd Msg
sendRefresh model =
    serialize model |> Router.getUserGames |> Http.send (Router.resolveResponse ReceiveEntries ReceiveError)



-- VIEW


view : Model -> List (Html Msg)
view model =
    [ th [ class "form-inline" ]
        [ div [ class "form-group" ]
            [ discountedInput model
            , input [ placeholder "Name", class "form-control", type_ "text", onInput ChangeName, value model.name ] []
            , sourcesSelect model
            , gameOnSelect model
            ]
        ]
    , th [ class "form-inline" ]
        [ div [ class "form-group" ]
            [ input [ placeholder "Lowest price", class "form-control", type_ "text", onInput ChangeLow, value <| Maybe.withDefault "" <| Maybe.map toString <| Tuple.first model.prices ] []
            , input [ placeholder "Highest price", class "form-control", type_ "text", onInput ChangeHigh, value <| Maybe.withDefault "" <| Maybe.map toString <| Tuple.second model.prices ] []
            ]
        ]
    , th [] [ button [ onClick Clear, class "glyphicon glyphicon-remove btn btn-default", style [ ( "float", "right" ) ] ] [] ]
    ]


gameOnSelect model =
    select [ class "form-control", onSelect ChangeGameOn ]
        [ option [ selected (model.gameOn == Nothing), value "" ] [ text "" ]
        , option [ selected (model.gameOn == Just Steam), value <| toString Steam ] [ text <| toString Steam ]
        , option [ selected (model.gameOn == Just Gog), value <| toString Gog ] [ text <| toString Gog ]
        ]


discountedInput model =
    div [ class "checkbox" ]
        [ label [] [ input [ type_ "checkbox", name "Discounted", checked model.isDiscounted, onCheck ChangeDiscounted ] [], text "Discounted" ]
        ]


sourcesSelect model =
    select [ class "form-control", onSelect ChangeSources ]
        [ option [ selected (model.sources == Owned), value <| toString Owned ] [ text <| toString Owned ]
        , option [ selected (model.sources == WishList), value <| toString WishList ] [ text <| toString WishList ]
        , option [ selected (model.sources == Both), value <| toString Both ] [ text <| toString Both ]
        ]
