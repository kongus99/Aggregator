port module MainPage exposing (..)
import Html exposing (Html, button, br, input, div, text, span, table, tr, th, td, select, option, a, label)
import Html.Attributes exposing(class, selected, value, href, type_, name)
import Html.Events exposing (onClick, on, targetValue, onInput)
import Json.Decode as Json
import GameEntry exposing(..)
import Http
import Task
import Erl
import Model exposing (..)
import Router exposing (..)

initProgram : String -> ( Model, Cmd Msg )
initProgram address =
    let
        parseInt value = String.toInt value |> Result.toMaybe |> Maybe.withDefault 0
        decodeAddress = Json.map2 (,) (Json.field "userId" <| Json.map parseInt Json.string) (Json.field "sources" <| Json.map sourcesFromString Json.string)
        (userId, sources) = Json.decodeString decodeAddress address |> Result.toMaybe |> Maybe.withDefault (initialModel.userId, initialModel.sources)
        model = {initialModel | sources = sources, userId = userId}
    in
        ( model , getResponse <| Router.getUserGames [("sources", toString model.sources), ("userId", toString model.userId)])

main = Html.programWithFlags { init = initProgram, view = view, update = update, subscriptions = \_ -> Sub.none }

-- PORTS
port elmAddressChange : String -> Cmd msg

-- MODEL

type alias Model = {sources : GameSources, message : String, userId : Int, filters : Filters}

initialModel = Model WishList "" 1 emptyFilters

-- UPDATE

type Msg
  = ChangeSources GameSources
  | SendRefresh (Cmd Msg)
  | ReceiveRefresh (List GameEntry)
  | RefreshError Http.Error
  | NameFilterChange String
  | LowPriceFilterChange String
  | HighPriceFilterChange String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeSources s -> ({model | filters = resetFilterLists model.filters, sources = s, message = ""}, getResponse <| Router.getUserGames [("sources", toString s), ("userId", toString model.userId)])
    SendRefresh cmd -> ({model | filters = resetFilterLists model.filters, message = ""}, cmd)
    ReceiveRefresh entries -> ({model | filters = updateFilterLists entries model.filters, message = ""} , Cmd.none)
    RefreshError err -> ({model | filters = resetFilterLists model.filters, message = toString err} , Cmd.none)
    NameFilterChange name -> ({model | filters = updateNameFilter name model.filters, message = ""} , Cmd.none)
    LowPriceFilterChange priceString -> ({model | filters = updateLowFilter (String.toFloat priceString |> Result.toMaybe) model.filters, message = ""} , Cmd.none)
    HighPriceFilterChange priceString -> ({model | filters = updateHighFilter (String.toFloat priceString |> Result.toMaybe) model.filters, message = ""} , Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div [] <|
    [ button [ onClick <| SendRefresh <| getResponse <| Router.refreshUserGames [("sources", toString model.sources), ("userId", toString model.userId)]] [ text "Refresh game data"   ]
    , br[][]
    , label[][text "Name:", input[type_ "text", onInput NameFilterChange, value model.filters.name][]]
    , label[][text "Lowest price:", input[type_ "text", onInput LowPriceFilterChange, value <| Maybe.withDefault "" <| Maybe.map toString <| Tuple.first model.filters.prices][]]
    , label[][text "Highest price:", input[type_ "text", onInput HighPriceFilterChange, value <| Maybe.withDefault "" <| Maybe.map toString <| Tuple.second model.filters.prices][]]
    , br[][]
    , div [] [sourcesSelect model.sources]
    , div [] [ text (toString model.message) ]
    , table[] <| gameTableTitle :: (List.map gameTableRow model.filters.result)
    ]

gameTableTitle =
    tr [] [ th[][text "Game - ", span[class "cell_Steam"][text " Steam"], span[class "cell_Gog"][text " Gog"], span[class "cell_Both"][text " Both"]]
          , th[][text "Price(PLN)"]
          , th[][text "Additional prices(PLN)"]
          ]

gameTableRow e =
    tr [] [ td[class <| toStyle e ][text <| getName e]
          , td[][text <| pricesToString (getPrice e)]
          , td[] (additionalPrices e.prices)
          ]

sourcesFromString value = case value of
                                "Owned" -> Owned
                                "WishList" -> WishList
                                _ -> Both

sourcesSelect sources =
    let
        change s = ChangeSources <| sourcesFromString s
    in
        select [onSelect change] [ option [selected (sources == Owned), value <| toString Owned][text <| toString Owned]
                                          , option [selected (sources == WishList), value <| toString WishList][text <| toString WishList]
                                          , option [selected (sources == Both), value <| toString Both][text <| toString Both]]

getResponse : (Http.Request (List GameEntry), String) -> Cmd Msg
getResponse (request, address) =
    Cmd.batch [Http.send (Router.resolveResponse ReceiveRefresh RefreshError) request, elmAddressChange address]

gamesOn list = List.map (\e -> if e == "Gog" then Gog else Steam) list

additionalPrices priceEntries =
    let
        price e = div[][ div [][text e.name]
                       , div [][a[href e.link][text e.host]]
                       , div [][text <| roundToString 2 e.price]]
    in
        List.map price priceEntries

toStyle gameEntry =
    let
        onGog = List.length gameEntry.gog > 0
        onSteam = List.length gameEntry.steam > 0
    in
        if onGog && onSteam then "cell_Both" else if onGog then "cell_Gog" else "cell_Steam"

onSelect : (String -> a) -> Html.Attribute a
onSelect msg =
    on "change" (Json.map msg targetValue)