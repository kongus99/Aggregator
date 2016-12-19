port module MainPage exposing (..)
import Html exposing (Html, button, div, text, span, table, tr, th, td, select, option, a)
import Html.Attributes exposing(class, selected, value, href)
import Html.Events exposing (onClick, on, targetValue)
import Json.Decode as Json
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
    in
        ( {initialModel | sources = sources, userId = userId}, Router.allData [("sources", toString initialModel.sources), ("userId", toString initialModel.userId)] |> Tuple.first |> sendRequest)

main = Html.programWithFlags { init = initProgram, view = view, update = update, subscriptions = \_ -> Sub.none }

-- PORTS
port elmAddressChange : String -> Cmd msg

-- MODEL

type GameSources = Owned | WishList | Both

type alias Model = {sources : GameSources, entries : List GameEntry, message : String, userId : Int}

initialModel = Model WishList [] "" 1

-- UPDATE

type Msg
  = ChangeSources GameSources
  | SendRefresh (Cmd Msg)
  | ReceiveRefresh (List GameEntry)
  | RefreshError Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeSources s -> ({model | entries = [], sources = s}, getResponse <| Router.allData [("sources", toString s), ("userId", toString model.userId)])
    SendRefresh cmd -> ({model | entries = []}, cmd)
    ReceiveRefresh entries -> ({model | entries = entries} , Cmd.none)
    RefreshError err -> ({model | entries = [], message = toString err} , Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div [] <|
    [ button [ onClick <| SendRefresh <| getResponse <| Router.gogData [("sources", toString model.sources), ("userId", toString model.userId)]] [ text "Fetch from gog"   ]
    , button [ onClick <| SendRefresh <| getResponse <| Router.steamData [("sources", toString model.sources), ("userId", toString model.userId)]] [ text "Fetch from steam" ]
    , div [] [sourcesSelect model.sources]
    , div [] [ text (toString model.message) ]
    , table[] <| gameTableTitle :: (List.map gameTableRow model.entries)
    ]

gameTableTitle =
    tr [] [ th[][text "Game"]
          , th[][text "Price(PLN)"]
          , th[][text "Additional prices(PLN)"]
          , th[][text "Gog/Steam/Both"]
          ]

gameTableRow e =
    tr [] [ td[][text <| getName e]
          , td[][text <| pricesToString (getPrice e)]
          , td[] (additionalPrices e.prices)
          , td[class <| toStyle e  ](toText e)
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
sendRequest request =
    Http.send (Router.resolveResponse ReceiveRefresh RefreshError) request

getResponse : (Http.Request (List GameEntry), String) -> Cmd Msg
getResponse (request, address) =
    Cmd.batch [sendRequest request, elmAddressChange address]

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
getPrice : GameEntry -> Maybe (Maybe Float, Maybe Float)
getPrice gameEntry =
    let
        steamPrice =  List.head gameEntry.steam |> Maybe.map (\s -> (s.price, s.discounted))
        gogPrice =  List.head gameEntry.gog |> Maybe.map (\g -> (g.price, g.discounted))
    in
        case gogPrice of
            Just x -> Just x
            Nothing -> steamPrice
roundToString : Int -> Float -> String
roundToString precision number =
    let
        integerRepresentation = number * toFloat (10 ^ precision) |> round |> toString
        total = String.dropRight 2 integerRepresentation
        fraction = String.dropLeft (String.length total) integerRepresentation
    in
        total ++ "." ++ fraction
pricesToString : Maybe (Maybe Float, Maybe Float) -> String
pricesToString prices =
    let
        calculatePercentage (price, discount) =
            Maybe.withDefault 0 <| Maybe.map2 (\p -> \d -> round (((p - d) / p) * 100)) price discount
        formatDiscount percentage price discount =
            (roundToString 2 price) ++ " (-" ++ toString percentage ++ "%) " ++ (roundToString 2 discount)
        convertToText percentage (price, discount) =
            if isNaN <| toFloat percentage then
                "0"
            else if percentage > 0 then
                Maybe.withDefault "Error" <| Maybe.map2 (formatDiscount percentage) price discount
            else
                Maybe.withDefault "" <| Maybe.map (roundToString 2) price
        discountPercentage =  Maybe.map calculatePercentage prices
    in
        Maybe.withDefault "" <| Maybe.map2 convertToText discountPercentage prices

getName gameEntry =
    let
        steamName = List.head gameEntry.steam |> Maybe.map (\g -> g.name) |> Maybe.withDefault ""
    in
        List.head gameEntry.gog |> Maybe.map (\g -> g.title) |> Maybe.withDefault steamName

toText gameEntry =
    let
        onGogNumber = List.length gameEntry.gog
        onSteamNumber = List.length gameEntry.steam
        onGogSpan = toSpan onGogNumber "gog_number"
        onSteamSpan = toSpan onSteamNumber "steam_number"
    in
        [onGogSpan, onSteamSpan]
toSpan n styleClass =
    if n > 1 then span[class styleClass ][text <| toString n] else span[][]

onSelect : (String -> a) -> Html.Attribute a
onSelect msg =
    on "change" (Json.map msg targetValue)