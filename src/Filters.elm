module Filters exposing (Model, Msg, initialize, replace, serialize, subscriptions, update, view)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Form.Input as Input
import Bootstrap.Navbar as Navbar
import CommonNavbar
import Erl
import GameEntry exposing (GameEntry, GameEntryRow, WebSocketRefreshResult, toGameEntryRow)
import Html exposing (Html, button, div, input, label, option, select, text, th, thead)
import Html.Attributes exposing (checked, class, href, multiple, name, placeholder, selected, style, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput)
import HtmlHelpers exposing (onLinkClick, onMenuItemCheck, onMenuItemClick, onMultiSelect, onSelect)
import Http
import Maybe.Extra as Maybes
import Model exposing (..)
import Navigation
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
    , isDeal : Maybe Bool
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


parse : Navbar.State -> Navigation.Location -> Model
parse navbarState loc =
    let
        url =
            Erl.parse loc.search

        userId =
            Router.extractSingleParam "userId" Parser.parseInt url |> Maybe.withDefault 1

        sources =
            Router.extractSingleParam "sources" Parser.parseSources url |> Maybe.withDefault WishList

        discounted =
            Router.extractSingleParam "discounted" Parser.parseBool url |> Maybe.withDefault False

        deal =
            Router.extractSingleParam "deal" Parser.parseBool url

        gameOn =
            Router.extractSingleParam "gameOn" Parser.parseGameOn url

        name =
            Router.extractSingleParam "name" Just url |> Maybe.withDefault ""

        genres =
            Router.extractParams "genres" Set.fromList url

        tags =
            Router.extractParams "tags" Set.fromList url

        lowPrice =
            Router.extractSingleParam "lowPrice" Parser.parseFloat url

        highPrice =
            Router.extractSingleParam "highPrice" Parser.parseFloat url
    in
    Model userId sources discounted deal gameOn name (initDynamicFilter genres) (initDynamicFilter tags) (PriceRange lowPrice highPrice) [] [] navbarState Nothing


initialize : Navigation.Location -> ( Model, Cmd Msg )
initialize loc =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

        model =
            parse navbarState loc
    in
    ( model, Cmd.batch [ navbarCmd, sendRefresh model ] )


serialize : Model -> List ( String, String )
serialize model =
    [ ( "sources", toString model.sources |> Just )
    , ( "userId", toString model.userId |> Just )
    , ( "discounted", toString model.isDiscounted |> Just )
    , ( "deal", Maybe.map toString model.isDeal )
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
        filter e =
            discountedFilter model.isDiscounted e
                && dealFilter model.isDeal e
                && gameOnFilter model.gameOn e
                && nameFilter model.name e
                && priceFilter model.range e
                && multiFilter (\e -> e.genres.value) model.genresFilter e
                && multiFilter (\e -> e.tags.value) model.tagsFilter e
    in
    { model | result = List.filter filter model.original, err = Nothing }


discountedFilter : Bool -> GameEntryRow -> Bool
discountedFilter isDiscounted entry =
    not isDiscounted || (priceExtractor entry |> Maybe.map Price.isDiscounted |> Maybe.withDefault False)


dealFilter : Maybe Bool -> GameEntryRow -> Bool
dealFilter isDeal entry =
    let
        f e =
            filterByAlternatePrices (List.head e.alternatePrices) (priceExtractor e)
    in
    isDeal
        |> Maybe.map
            (\d ->
                if d then
                    f entry
                else
                    not (f entry)
            )
        |> Maybe.withDefault True


gameOnFilter : Maybe GameOn -> GameEntryRow -> Bool
gameOnFilter gameOn entry =
    gameOn
        |> Maybe.map (\on -> Maybes.isNothing entry.gameOn || entry.gameOn == Just on)
        |> Maybe.withDefault True


nameFilter : String -> GameEntryRow -> Bool
nameFilter name entry =
    String.isEmpty name || (entry.name |> String.toLower |> String.contains (String.toLower name))


multiFilter : (GameEntryRow -> List String) -> DynamicFilter -> GameEntryRow -> Bool
multiFilter entryValues filter entry =
    Set.isEmpty filter.selectedValues
        || (let
                condition values =
                    if not filter.conjunction then
                        values |> Set.intersect filter.selectedValues |> Set.isEmpty |> not
                    else
                        (Set.union values filter.selectedValues |> Set.size) == (values |> Set.size)
            in
            entry |> entryValues |> Set.fromList |> condition
           )


priceFilter : PriceRange -> GameEntryRow -> Bool
priceFilter range entry =
    filterByPriceRange range (priceExtractor entry)


priceExtractor entry =
    entry.price |> Maybe.map .value


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navbarState NavbarMsg



-- UPDATE


type Msg
    = ChangeName String
    | ChangeLow String
    | ChangeHigh String
    | ChangeSelectedGenre String Bool
    | ChangeSelectedTag String Bool
    | ChangeGameOn String
    | ChangeDiscounted Bool
    | ChangeDeal Bool
    | ChangeNotDeal Bool
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

        ChangeDeal _ ->
            apply { model | isDeal = resolveDeal model.isDeal True } ! []

        ChangeNotDeal _ ->
            apply { model | isDeal = resolveDeal model.isDeal False } ! []

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


resolveDeal currentDeal newDeal =
    case currentDeal of
        Just True ->
            if newDeal then
                Nothing
            else
                Just False

        Just False ->
            if newDeal then
                Just True
            else
                Nothing

        Nothing ->
            Just newDeal


sendRefresh : Model -> Cmd Msg
sendRefresh model =
    serialize model |> routes.main.fetch |> .request |> Http.send (Router.resolveResponse ReceiveEntries ReceiveError)



-- VIEW


view : Model -> Html Msg
view model =
    CommonNavbar.navbar CommonNavbar.Main NavbarMsg (Navbar.items [ pricingDropdown model, filtersDropdown model, genresDropdown model, tagsDropdown model ]) |> Navbar.view model.navbarState


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
                    , ButtonGroup.checkboxButton (model.isDeal |> Maybe.withDefault False) [ Button.attrs [ onMenuItemCheck ChangeDeal ], Button.secondary ] [ text "Deal" ]
                    , ButtonGroup.checkboxButton (model.isDeal |> Maybe.map not |> Maybe.withDefault False) [ Button.attrs [ onMenuItemCheck ChangeNotDeal ], Button.secondary ] [ text "Not deal" ]
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
