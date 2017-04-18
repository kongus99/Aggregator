module Filters exposing (Model, parse, Msg, update, refresh, view, serialize, replace)

import Html exposing (Html, button, div, input, label, option, select, text, th)
import Html.Attributes exposing (checked, class, multiple, name, placeholder, selected, style, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput)
import HtmlHelpers exposing (onMultiSelect, onSelect)
import Http
import Model exposing (..)
import GameEntry exposing (GameEntry, WebSocketRefreshResult, GameEntryRow, toGameEntryRow)
import Erl
import Parser
import Router exposing (routes)
import Set exposing (Set)
import WebSocket


-- MODEL


type alias Model =
    { userId : Int
    , sources : GameSources
    , isDiscounted : Bool
    , gameOn : Maybe GameOn
    , name : String
    , genresFilter : DynamicFilter
    , tagsFilter : DynamicFilter
    , prices : ( Maybe Float, Maybe Float )
    , original : List GameEntryRow
    , result : List GameEntryRow
    , err : Maybe Http.Error
    }


type alias DynamicFilter =
    { allValues : Set String, selectedValues : Set String, includeEmpty : Bool }


initDynamicFilter : Set String -> DynamicFilter
initDynamicFilter selected =
    DynamicFilter Set.empty selected True


updateSelectedDynamicFilter : Set String -> DynamicFilter -> DynamicFilter
updateSelectedDynamicFilter values filter =
    { filter | selectedValues = values }


replace : WebSocketRefreshResult -> Model -> Model
replace r model =
    { model | original = GameEntry.update r model.sources model.original } |> regenerateDynamicFilters |> apply


regenerateDynamicFilters : Model -> Model
regenerateDynamicFilters model =
    let
        updateDynamicFilter all oldFilter =
            { oldFilter | allValues = all, selectedValues = Set.intersect all oldFilter.selectedValues }

        newValues values =
            model.original |> List.map (\e -> values e |> Set.fromList) |> List.foldl Set.union Set.empty

        newGenresFilter =
            updateDynamicFilter (newValues (\v -> v.genres.value)) model.genresFilter

        newTagsFilter =
            updateDynamicFilter (newValues (\v -> v.tags.value)) model.tagsFilter
    in
        { model | genresFilter = newGenresFilter, tagsFilter = newTagsFilter }


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

        genres =
            Erl.getQueryValuesForKey "genres" url |> Set.fromList

        tags =
            Erl.getQueryValuesForKey "tags" url |> Set.fromList

        lowPrice =
            Erl.getQueryValuesForKey "lowPrice" url |> List.head |> Maybe.andThen Parser.parseFloat

        highPrice =
            Erl.getQueryValuesForKey "highPrice" url |> List.head |> Maybe.andThen Parser.parseFloat
    in
        Model userId sources discounted gameOn name (initDynamicFilter genres) (initDynamicFilter tags) ( lowPrice, highPrice ) [] [] Nothing


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
        |> List.append (model.genresFilter.selectedValues |> Set.toList |> List.map (\g -> ( "genres", Just g )))
        |> List.append (model.tagsFilter.selectedValues |> Set.toList |> List.map (\t -> ( "tags", Just t )))
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
                |> applyMultiFilter (\e -> e.genres.value) model.genresFilter.selectedValues
                |> applyMultiFilter (\e -> e.tags.value) model.tagsFilter.selectedValues
    in
        { model | result = result, err = Nothing }


applyDiscountedFilter : Bool -> List GameEntryRow -> List GameEntryRow
applyDiscountedFilter isDiscounted entries =
    let
        filterDiscounted e =
            Maybe.map (\p -> not (Tuple.second p == Nothing)) (e.prices |> Maybe.map .value) |> Maybe.withDefault False
    in
        if isDiscounted then
            List.filter filterDiscounted entries
        else
            entries


applyGameOnFilter : Maybe GameOn -> List GameEntryRow -> List GameEntryRow
applyGameOnFilter gameOn entries =
    let
        isOn entry =
            gameOn
                |> Maybe.map
                    (\on ->
                        if entry.gameOn == Nothing then
                            True
                        else
                            entry.gameOn == Just on
                    )
                |> Maybe.withDefault True
    in
        List.filter isOn entries


applyNameFilter : String -> List GameEntryRow -> List GameEntryRow
applyNameFilter name entries =
    if String.isEmpty name then
        entries
    else
        List.filter (\e -> e.name |> String.toLower |> String.contains (String.toLower name)) entries


applyMultiFilter : (GameEntryRow -> List String) -> Set String -> List GameEntryRow -> List GameEntryRow
applyMultiFilter entryValues newValues entries =
    if Set.isEmpty newValues then
        entries
    else
        List.filter (\e -> entryValues e |> Set.fromList |> Set.intersect newValues |> Set.isEmpty |> not) entries


applyPriceFilter : ( Maybe Float, Maybe Float ) -> List GameEntryRow -> List GameEntryRow
applyPriceFilter ( lowPrice, highPrice ) entries =
    let
        filterByLow lowPrice entry =
            entry.prices |> Maybe.map .value |> discountedIfAvailable |> Maybe.map (\e -> e >= lowPrice) |> Maybe.withDefault False

        filterByHigh highPrice entry =
            entry.prices |> Maybe.map .value |> discountedIfAvailable |> Maybe.map (\e -> e <= highPrice) |> Maybe.withDefault False

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
    | ChangeSelectedGenres (List String)
    | ChangeSelectedTags (List String)
    | ChangeGameOn String
    | ChangeDiscounted Bool
    | ChangeSources String
    | ReceiveEntries (List GameEntry)
    | ReceiveError Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clear ->
            apply (Model model.userId model.sources False Nothing "" (updateSelectedDynamicFilter Set.empty model.genresFilter) (updateSelectedDynamicFilter Set.empty model.genresFilter) ( Nothing, Nothing ) model.original [] Nothing) ! []

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

        ChangeSelectedGenres genres ->
            ({ model | genresFilter = updateSelectedDynamicFilter (Set.fromList genres) model.genresFilter } |> apply) ! []

        ChangeSelectedTags tags ->
            ({ model | tagsFilter = updateSelectedDynamicFilter (Set.fromList tags) model.tagsFilter } |> apply) ! []

        ReceiveEntries entries ->
            ({ model | original = List.map toGameEntryRow entries } |> regenerateDynamicFilters |> apply) ! []

        ReceiveError err ->
            { model | err = Just err } ! []


refresh : String -> Model -> ( Model, Cmd Msg )
refresh s model =
    ( model, sendRefresh model )


sendRefresh : Model -> Cmd Msg
sendRefresh model =
    serialize model |> routes.main.fetch |> .request |> Http.send (Router.resolveResponse ReceiveEntries ReceiveError)



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
    , th [ class "form-inline" ] [ genresSelect model ]
    , th [ class "form-inline" ] [ tagsSelect model ]
    , th [ class "form-inline" ]
        [ div [ class "form-group" ]
            [ input [ placeholder "Lowest price", class "form-control", type_ "text", onInput ChangeLow, value <| Maybe.withDefault "" <| Maybe.map toString <| Tuple.first model.prices ] []
            , input [ placeholder "Highest price", class "form-control", type_ "text", onInput ChangeHigh, value <| Maybe.withDefault "" <| Maybe.map toString <| Tuple.second model.prices ] []
            ]
        ]
    , th [] [ button [ onClick Clear, class "glyphicon glyphicon-remove btn btn-default", style [ ( "float", "right" ) ] ] [] ]
    ]


genresSelect model =
    select [ class "form-control", multiple True, onMultiSelect ChangeSelectedGenres ] <| List.map (dynamicOptions model.genresFilter.selectedValues) (Set.toList model.genresFilter.allValues)


tagsSelect model =
    select [ class "form-control", multiple True, onMultiSelect ChangeSelectedTags ] <| List.map (dynamicOptions model.tagsFilter.selectedValues) (Set.toList model.tagsFilter.allValues)


dynamicOptions selectedSet currentValue =
    option [ selected (Set.member currentValue selectedSet), value currentValue ] [ text currentValue ]


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
