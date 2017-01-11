module GameEntry exposing (GameEntry, Filters, getPrice, resetFilterLists, setNewFilterLists, getName, pricesToString, roundToString, emptyFilters, filterByName)
import Model exposing (..)

type alias GameEntry = {gog: List GogEntry, steam: List SteamEntry, prices : List PriceEntry}
type alias Filters = {name : String, prices : (Maybe Float, Maybe Float), original : List GameEntry, result : List GameEntry}

emptyFilters = Filters "" (Nothing, Nothing) [] []

getName : GameEntry -> String
getName gameEntry =
    let
        steamName = List.head gameEntry.steam |> Maybe.map (\g -> g.name) |> Maybe.withDefault ""
    in
        List.head gameEntry.gog |> Maybe.map (\g -> g.title) |> Maybe.withDefault steamName

getPrice : GameEntry -> Maybe (Maybe Float, Maybe Float)
getPrice gameEntry =
    let
        steamPrice =  List.head gameEntry.steam |> Maybe.map (\s -> (s.price, s.discounted))
        gogPrice =  List.head gameEntry.gog |> Maybe.map (\g -> (g.price, g.discounted))
    in
        case gogPrice of
            Just x -> Just x
            Nothing -> steamPrice

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

roundToString : Int -> Float -> String
roundToString precision number =
    let
        integerRepresentation = number * toFloat (10 ^ precision) |> round |> toString
        total = String.dropRight 2 integerRepresentation
        fraction = String.dropLeft (String.length total) integerRepresentation
    in
        total ++ "." ++ fraction

resetFilterLists : Filters -> Filters
resetFilterLists filters =
    {filters | result = [], original = [] }

setNewFilterLists : List GameEntry -> Filters -> Filters
setNewFilterLists list filters =
    {filters | result = applyNameFilter filters.name list, original = list }

applyNameFilter name entries =
    if String.isEmpty name then entries
    else List.filter (\e -> getName e |> String.toLower |> String.contains (String.toLower name)) entries

filterByName : String -> Filters -> Filters
filterByName name filters = {filters | name = name, result = applyNameFilter name filters.original}

--filterByLowerPrice : Maybe Float -> List GameEntry -> Filters -> Filters
--filterByLowerPrice price entries filters =
--    let
--        applyPriceFilter =
--            if price == Nothing then entries
--            else List.filter (\e -> getName e |> String.toLower |> String.contains (String.toLower name)) list
--        updatedPrices (lower, upper) = (price, upper)
--    in
--        {filters | prices = updatedPrices filters.prices, result = applyPriceFilter entries}