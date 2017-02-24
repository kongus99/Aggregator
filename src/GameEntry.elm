module GameEntry
    exposing
        ( GameEntry
        , Filters
        , getPrice
        , resetFilterLists
        , updateFilterLists
        , getName
        , pricesToString
        , roundToString
        , emptyFilters
        , updateNameFilter
        , updateLowFilter
        , updateHighFilter
        , updateGameOnFilter
        , toggleDiscountedFilter
        , getSteamId
        , getLink
        )

import Model exposing (..)


type alias GameEntry =
    { gog : List GogEntry, steam : List SteamEntry, prices : List PriceEntry }


type alias Filters =
    { isDiscounted : Bool, gameOn : Maybe GameOn, name : String, prices : ( Maybe Float, Maybe Float ), original : List GameEntry, result : List GameEntry }


emptyFilters =
    Filters False Nothing "" ( Nothing, Nothing ) [] []


getName : GameEntry -> String
getName gameEntry =
    let
        steamName =
            List.head gameEntry.steam |> Maybe.map (\g -> g.name) |> Maybe.withDefault ""
    in
        List.head gameEntry.gog |> Maybe.map (\g -> g.title) |> Maybe.withDefault steamName


getLink : GameEntry -> String
getLink gameEntry =
    let
        steamLink =
            List.head gameEntry.steam |> Maybe.map (\g -> g.link) |> Maybe.withDefault ""
    in
        List.head gameEntry.gog |> Maybe.map (\g -> g.link) |> Maybe.withDefault steamLink


getSteamId : GameEntry -> Maybe Int
getSteamId gameEntry =
    List.head gameEntry.steam |> Maybe.map (\g -> g.steamId)


getPrice : GameEntry -> Maybe ( Maybe Float, Maybe Float )
getPrice gameEntry =
    let
        steamPrice =
            List.head gameEntry.steam |> Maybe.map (\s -> ( s.price, s.discounted ))

        gogPrice =
            List.head gameEntry.gog |> Maybe.map (\g -> ( g.price, g.discounted ))
    in
        case gogPrice of
            Just x ->
                Just x

            Nothing ->
                steamPrice


pricesToString : Maybe ( Maybe Float, Maybe Float ) -> String
pricesToString prices =
    let
        calculatePercentage ( price, discount ) =
            Maybe.withDefault 0 <| Maybe.map2 (\p -> \d -> round (((p - d) / p) * 100)) price discount

        formatDiscount percentage price discount =
            (roundToString 2 price) ++ " (-" ++ toString percentage ++ "%) " ++ (roundToString 2 discount)

        convertToText percentage ( price, discount ) =
            if isNaN <| toFloat percentage then
                "0"
            else if percentage > 0 then
                Maybe.withDefault "Error" <| Maybe.map2 (formatDiscount percentage) price discount
            else
                Maybe.withDefault "" <| Maybe.map (roundToString 2) price

        discountPercentage =
            Maybe.map calculatePercentage prices
    in
        Maybe.withDefault "" <| Maybe.map2 convertToText discountPercentage prices


roundToString : Int -> Float -> String
roundToString precision number =
    let
        integerRepresentation =
            number * toFloat (10 ^ precision) |> round |> toString

        total =
            String.dropRight 2 integerRepresentation

        fraction =
            String.dropLeft (String.length total) integerRepresentation
    in
        total ++ "." ++ fraction


resetFilterLists : Filters -> Filters
resetFilterLists filters =
    { filters | result = [], original = [] }


updateFilterLists : List GameEntry -> Filters -> Filters
updateFilterLists list filters =
    applyFilters { filters | original = list }


updateNameFilter : String -> Filters -> Filters
updateNameFilter name filters =
    applyFilters { filters | name = name }


updateLowFilter : Maybe Float -> Filters -> Filters
updateLowFilter low filters =
    applyFilters { filters | prices = ( low, Tuple.second filters.prices ) }


updateHighFilter : Maybe Float -> Filters -> Filters
updateHighFilter high filters =
    applyFilters { filters | prices = ( Tuple.first filters.prices, high ) }


updateGameOnFilter : Maybe GameOn -> Filters -> Filters
updateGameOnFilter on filters =
    applyFilters { filters | gameOn = on }


toggleDiscountedFilter : Bool -> Filters -> Filters
toggleDiscountedFilter isDiscounted filters =
    applyFilters { filters | isDiscounted = isDiscounted }


applyFilters : Filters -> Filters
applyFilters filters =
    let
        result =
            applyDiscountedFilter filters.isDiscounted filters.original
                |> applyGameOnFilter filters.gameOn
                |> applyNameFilter filters.name
                |> applyPriceFilter filters.prices
    in
        { filters | result = result }


applyDiscountedFilter : Bool -> List GameEntry -> List GameEntry
applyDiscountedFilter isDiscounted entries =
    let
        filterDiscounted e =
            Maybe.map (\p -> not (Tuple.second p == Nothing)) (getPrice e) |> Maybe.withDefault False
    in
        if not isDiscounted then
            entries
        else
            List.filter filterDiscounted entries


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
