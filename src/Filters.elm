module Filters exposing (Model, Msg, initialize, replace, serialize, subscriptions, update, view)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Form.Input as Input
import Bootstrap.Navbar as Navbar
import Erl
import GameEntry exposing (GameEntry, GameEntryRow, WebSocketRefreshResult, toGameEntryRow)
import Html exposing (Html, button, div, input, label, option, select, text, th, thead)
import Html.Attributes exposing (checked, class, href, multiple, name, placeholder, selected, style, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput)
import HtmlHelpers exposing (onLinkClick, onMenuItemCheck, onMenuItemClick, onMultiSelect, onSelect)
import Http
import Model exposing (..)
import Parser
import Price exposing (AlternatePrice, Price, PriceRange, filterByAlternatePrices, filterByPriceRange, updateHighRange, updateLowRange)
import Router exposing (routes)
import Set exposing (Set)
import WebSocket


-- MODEL


type alias Model =
    { userId : Int
    , sources : GameSources
    , isDiscounted : Bool
    , isDeal : Bool
    , gameOn : Maybe GameOn
    , name : String
    , genresFilter : DynamicFilter
    , tagsFilter : DynamicFilter
    , range : PriceRange
    , original : List GameEntryRow
    , result : List GameEntryRow
    , navbarState : Navbar.State
    , err : Maybe Http.Error
    }


type alias DynamicFilter =
    { allValues : Set String, selectedValues : Set String, conjunction : Bool }


initDynamicFilter : Set String -> DynamicFilter
initDynamicFilter selected =
    DynamicFilter Set.empty selected False


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


parse : Navbar.State -> Erl.Url -> Model
parse navbarState url =
    let
        userId =
            Erl.getQueryValuesForKey "userId" url |> List.head |> Maybe.andThen Parser.parseInt |> Maybe.withDefault 1

        sources =
            Erl.getQueryValuesForKey "sources" url |> List.head |> Maybe.andThen Parser.parseSources |> Maybe.withDefault WishList

        discounted =
            Erl.getQueryValuesForKey "discounted" url |> List.head |> Maybe.andThen Parser.parseBool |> Maybe.withDefault False

        deal =
            Erl.getQueryValuesForKey "deal" url |> List.head |> Maybe.andThen Parser.parseBool |> Maybe.withDefault False

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
    Model userId sources discounted deal gameOn name (initDynamicFilter genres) (initDynamicFilter tags) (PriceRange lowPrice highPrice) [] [] navbarState Nothing


initialize : Erl.Url -> ( Model, Cmd Msg )
initialize url =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

        model =
            parse navbarState url
    in
    ( model, Cmd.batch [ navbarCmd, sendRefresh model ] )


serialize : Model -> List ( String, String )
serialize model =
    [ ( "sources", toString model.sources |> Just )
    , ( "userId", toString model.userId |> Just )
    , ( "discounted", toString model.isDiscounted |> Just )
    , ( "deal", toString model.isDeal |> Just )
    , ( "gameOn", Maybe.map toString model.gameOn )
    , ( "name", model.name |> Just )
    , ( "lowPrice", model.range.low |> Maybe.map toString )
    , ( "highPrice", model.range.high |> Maybe.map toString )
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
                |> applyDealFilter model.isDeal
                |> applyGameOnFilter model.gameOn
                |> applyNameFilter model.name
                |> applyPriceFilter model.range
                |> applyMultiFilter (\e -> e.genres.value) model.genresFilter
                |> applyMultiFilter (\e -> e.tags.value) model.tagsFilter
    in
    { model | result = result, err = Nothing }


applyDiscountedFilter : Bool -> List GameEntryRow -> List GameEntryRow
applyDiscountedFilter isDiscounted entries =
    let
        filterDiscounted e =
            priceExtractor e |> Maybe.map Price.isDiscounted |> Maybe.withDefault False
    in
    if isDiscounted then
        List.filter filterDiscounted entries
    else
        entries


applyDealFilter : Bool -> List GameEntryRow -> List GameEntryRow
applyDealFilter isDeal entries =
    if isDeal then
        filterByAlternatePrices (\ge -> List.head ge.alternatePrices) priceExtractor entries
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


applyMultiFilter : (GameEntryRow -> List String) -> DynamicFilter -> List GameEntryRow -> List GameEntryRow
applyMultiFilter entryValues filter entries =
    if Set.isEmpty filter.selectedValues then
        entries
    else
        let
            condition values =
                if not filter.conjunction then
                    values |> Set.intersect filter.selectedValues |> Set.isEmpty |> not
                else
                    (Set.union values filter.selectedValues |> Set.size) == (values |> Set.size)
        in
        List.filter (\e -> entryValues e |> Set.fromList |> condition) entries


applyPriceFilter : PriceRange -> List GameEntryRow -> List GameEntryRow
applyPriceFilter range entries =
    filterByPriceRange range priceExtractor entries


priceExtractor entry =
    entry.price |> Maybe.map .value


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navbarState NavbarMsg



-- UPDATE


type Msg
    = Clear
    | ChangeName String
    | ChangeLow String
    | ChangeHigh String
    | ChangeSelectedGenre String Bool
    | ChangeSelectedTag String Bool
    | ChangeGameOn String
    | ChangeDiscounted Bool
    | ChangeDeal Bool
    | ChangeTagsConjunction Bool
    | ChangeGenresConjunction Bool
    | ChangeSources String
    | ReceiveEntries (List GameEntry)
    | ReceiveError Http.Error
    | NavbarMsg Navbar.State
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clear ->
            let
                genresFilter =
                    DynamicFilter model.genresFilter.allValues Set.empty model.genresFilter.conjunction

                tagsFilter =
                    DynamicFilter model.tagsFilter.allValues Set.empty model.tagsFilter.conjunction
            in
            apply (Model model.userId model.sources False False Nothing "" genresFilter tagsFilter (PriceRange Nothing Nothing) model.original [] model.navbarState Nothing) ! []

        ChangeName name ->
            apply { model | name = name } ! []

        ChangeLow low ->
            apply { model | range = updateLowRange (Parser.parseFloat low) model.range } ! []

        ChangeHigh high ->
            apply { model | range = updateHighRange (Parser.parseFloat high) model.range } ! []

        ChangeGameOn on ->
            apply { model | gameOn = Parser.parseGameOn on } ! []

        ChangeDiscounted isDiscounted ->
            apply { model | isDiscounted = isDiscounted } ! []

        ChangeDeal isDeal ->
            apply { model | isDeal = isDeal } ! []

        ChangeSources s ->
            let
                newModel =
                    apply { model | result = [], original = [], sources = Parser.parseSources s |> Maybe.withDefault Both }
            in
            newModel ! [ sendRefresh newModel ]

        ChangeSelectedGenre genre isAdded ->
            let
                selected =
                    if isAdded then
                        Set.insert genre model.genresFilter.selectedValues
                    else
                        Set.remove genre model.genresFilter.selectedValues
            in
            apply { model | genresFilter = DynamicFilter model.genresFilter.allValues selected model.genresFilter.conjunction } ! []

        ChangeSelectedTag tag isAdded ->
            let
                selected =
                    if isAdded then
                        Set.insert tag model.tagsFilter.selectedValues
                    else
                        Set.remove tag model.tagsFilter.selectedValues
            in
            apply { model | tagsFilter = DynamicFilter model.tagsFilter.allValues selected model.tagsFilter.conjunction } ! []

        ChangeTagsConjunction conjunction ->
            apply { model | tagsFilter = DynamicFilter model.tagsFilter.allValues model.tagsFilter.selectedValues conjunction } ! []

        ChangeGenresConjunction conjunction ->
            apply { model | genresFilter = DynamicFilter model.genresFilter.allValues model.genresFilter.selectedValues conjunction } ! []

        ReceiveEntries entries ->
            ({ model | original = List.map toGameEntryRow entries } |> regenerateDynamicFilters |> apply) ! []

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        NoOp ->
            model ! []

        ReceiveError err ->
            { model | err = Just err } ! []


sendRefresh : Model -> Cmd Msg
sendRefresh model =
    serialize model |> routes.main.fetch |> .request |> Http.send (Router.resolveResponse ReceiveEntries ReceiveError)



-- VIEW
--TODO - reset link should show proper address


view : Model -> Html Msg
view model =
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.attrs [ class "sticky-top" ]
        |> Navbar.brand [ href "", onLinkClick Clear ] [ text "Game Aggregator" ]
        |> Navbar.items [ pricingDropdown model, filtersDropdown model, genresDropdown model, tagsDropdown model ]
        |> Navbar.customItems [ logout ]
        |> Navbar.view model.navbarState


logout =
    Navbar.textItem []
        [ Button.linkButton
            [ Button.outlineSecondary
            , Button.attrs [ class "fa fa-link", href "/comparison" ]
            ]
            []
        , Button.linkButton
            [ Button.outlineSecondary
            , Button.attrs [ class "fa fa-sign-out", href "/" ]
            ]
            []
        ]


filtersDropdown model =
    Navbar.dropdown
        { id = "Filters"
        , toggle = Navbar.dropdownToggle [] [ text "Filters" ]
        , items =
            [ Navbar.dropdownItem [ onMenuItemClick NoOp ] [ Input.text [ Input.placeholder "Name", Input.onInput ChangeName, Input.value model.name ] ]
            , Navbar.dropdownDivider
            , Navbar.dropdownHeader [ text "Source" ]
            , Navbar.dropdownItem [ onMenuItemClick NoOp ]
                [ ButtonGroup.radioButtonGroup [ ButtonGroup.small ]
                    [ ButtonGroup.radioButton (model.sources == WishList) [ Button.attrs [ onMenuItemClick (ChangeSources <| toString WishList) ], Button.secondary ] [ text <| toString WishList ]
                    , ButtonGroup.radioButton (model.sources == Owned) [ Button.attrs [ onMenuItemClick (ChangeSources <| toString Owned) ], Button.secondary ] [ text <| toString Owned ]
                    , ButtonGroup.radioButton (model.sources == Both) [ Button.attrs [ onMenuItemClick (ChangeSources <| toString Both) ], Button.secondary ] [ text <| toString Both ]
                    ]
                ]
            , Navbar.dropdownDivider
            , Navbar.dropdownHeader [ text "Shop" ]
            , Navbar.dropdownItem [ onMenuItemClick NoOp ]
                [ ButtonGroup.radioButtonGroup [ ButtonGroup.small ]
                    [ ButtonGroup.radioButton (model.gameOn == Nothing) [ Button.attrs [ onMenuItemClick (ChangeGameOn "") ], Button.secondary ] [ text <| toString Both ]
                    , ButtonGroup.radioButton (model.gameOn == Just Steam) [ Button.attrs [ onMenuItemClick (ChangeGameOn <| toString Steam) ], Button.secondary ] [ text <| toString Steam ]
                    , ButtonGroup.radioButton (model.gameOn == Just Gog) [ Button.attrs [ onMenuItemClick (ChangeGameOn <| toString Gog) ], Button.secondary ] [ text <| toString Gog ]
                    ]
                ]
            ]
        }


pricingDropdown model =
    Navbar.dropdown
        { id = "Pricing"
        , toggle = Navbar.dropdownToggle [] [ text "Pricing" ]
        , items =
            [ Navbar.dropdownItem [ onMenuItemClick NoOp ]
                [ ButtonGroup.checkboxButtonGroup [ ButtonGroup.small ]
                    [ ButtonGroup.checkboxButton model.isDiscounted [ Button.attrs [ onMenuItemCheck ChangeDiscounted ], Button.secondary ] [ text "Discounted" ]
                    , ButtonGroup.checkboxButton model.isDeal [ Button.attrs [ onMenuItemCheck ChangeDeal ], Button.secondary ] [ text "Deal" ]
                    ]
                ]
            , Navbar.dropdownDivider
            , Navbar.dropdownItem [ onMenuItemClick NoOp ] [ Input.text [ Input.placeholder "Lowest", Input.onInput ChangeLow, Input.value <| Maybe.withDefault "" <| Maybe.map toString <| model.range.low ] ]
            , Navbar.dropdownDivider
            , Navbar.dropdownItem [ onMenuItemClick NoOp ] [ Input.text [ Input.placeholder "Highest", Input.onInput ChangeHigh, Input.value <| Maybe.withDefault "" <| Maybe.map toString <| model.range.high ] ]
            ]
        }


genresDropdown model =
    Navbar.dropdown
        { id = "Genres"
        , toggle = Navbar.dropdownToggle [] [ text "Genres" ]
        , items =
            [ Navbar.dropdownItem [ onMenuItemClick NoOp ]
                [ ButtonGroup.checkboxButtonGroup []
                    [ ButtonGroup.checkboxButton model.genresFilter.conjunction [ Button.attrs [ onMenuItemCheck ChangeGenresConjunction ], Button.secondary ] [ text "Conjunction" ]
                    ]
                ]
            , Navbar.dropdownDivider
            , Navbar.dropdownItem [ onMenuItemClick NoOp, class "scrollable-menu" ]
                [ ButtonGroup.checkboxButtonGroup [ ButtonGroup.vertical, ButtonGroup.small ] <|
                    List.map (dynamicOptions ChangeSelectedGenre model.genresFilter.selectedValues) (Set.toList model.genresFilter.allValues)
                ]
            ]
        }


tagsDropdown model =
    Navbar.dropdown
        { id = "Tags"
        , toggle = Navbar.dropdownToggle [] [ text "Tags" ]
        , items =
            [ Navbar.dropdownItem [ onMenuItemClick NoOp ]
                [ ButtonGroup.checkboxButtonGroup []
                    [ ButtonGroup.checkboxButton model.tagsFilter.conjunction [ Button.attrs [ onMenuItemCheck ChangeTagsConjunction ], Button.secondary ] [ text "Conjunction" ]
                    ]
                ]
            , Navbar.dropdownDivider
            , Navbar.dropdownItem [ onMenuItemClick NoOp, class "scrollable-menu" ]
                [ ButtonGroup.checkboxButtonGroup [ ButtonGroup.vertical, ButtonGroup.small ] <|
                    List.map (dynamicOptions ChangeSelectedTag model.tagsFilter.selectedValues) (Set.toList model.tagsFilter.allValues)
                ]
            ]
        }


dynamicOptions msg selectedSet currentValue =
    ButtonGroup.checkboxButton (Set.member currentValue selectedSet) [ Button.attrs [ onMenuItemCheck (msg currentValue) ], Button.secondary ] [ text currentValue ]
