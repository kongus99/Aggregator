module Price exposing (..)


type alias Price =
    { normal : Float, discounted : Maybe Float }


type alias PriceRange =
    { low : Maybe Float, high : Maybe Float }


type alias AlternatePrice =
    { steamId : Int, name : String, host : String, link : String, price : Float }


priceToString : Price -> String
priceToString price =
    let
        formatDiscounted discounted =
            let
                percentage =
                    (((price.normal - discounted) / price.normal) * 100) |> round
            in
                (roundToString 2 price.normal) ++ " (-" ++ toString percentage ++ "%) " ++ (roundToString 2 discounted)
    in
        price.discounted |> Maybe.map formatDiscounted |> Maybe.withDefault (roundToString 2 price.normal)


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


filterByPriceRange : PriceRange -> (a -> Maybe Price) -> List a -> List a
filterByPriceRange range extractor entries =
    let
        filter ( low, high ) price =
            price |> discountedIfAvailable |> Maybe.map (\p -> p >= low && p <= high) |> Maybe.withDefault False

        bounds =
            ( range.low |> Maybe.withDefault 0, range.high |> Maybe.withDefault 1000000 )
    in
        List.filter (\e -> extractor e |> (filter bounds)) entries


discountedIfAvailable : Maybe Price -> Maybe Float
discountedIfAvailable price =
    let
        selectPrice { normal, discounted } =
            if discounted == Nothing then
                Just normal
            else
                discounted
    in
        Maybe.andThen selectPrice price


isDiscounted : Price -> Bool
isDiscounted price =
    price.discounted == Nothing |> not


updateLowRange low range =
    { range | low = low }


updateHighRange high range =
    { range | high = high }
