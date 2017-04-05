module GameEntry
    exposing
        ( GameEntry
        , getPrice
        , getName
        , pricesToString
        , roundToString
        , getSteamId
        , getLink
        , update
        , WebSocketRefreshResult
        )

import Dict exposing (Dict)
import Dict.Extra as Dicts
import Model exposing (..)
import Set exposing (Set)


type alias GameEntry =
    { gog : List GogEntry, steam : List SteamEntry, prices : List PriceEntry }


type alias WishlistEntries =
    List GameEntry


type alias OwnedEntries =
    List GameEntry


type alias WebSocketRefreshResult =
    { games : Maybe ( WishlistEntries, OwnedEntries ), prices : Maybe ( List Int, List PriceEntry ) }


update : WebSocketRefreshResult -> GameSources -> List GameEntry -> List GameEntry
update newData sources oldData =
    updateGames newData sources oldData |> updatePrices newData sources


updatePrices : WebSocketRefreshResult -> GameSources -> List GameEntry -> List GameEntry
updatePrices newData sources oldData =
    if sources == Owned then
        oldData
    else
        newData.prices
            |> Maybe.map
                (\( steamIds, prices ) ->
                    let
                        existingGroupedPrices =
                            prices |> Dicts.groupBy .steamId |> Dict.map (\k -> \v -> List.sortBy .price v)

                        groupedPrices =
                            steamIds |> List.map (\id -> ( id, [] )) |> Dict.fromList |> Dict.union existingGroupedPrices
                    in
                        oldData
                            |> List.map
                                (\g ->
                                    let
                                        toReplace =
                                            getSteamId g
                                                |> Maybe.andThen (\id -> Dict.get id groupedPrices)
                                                |> Maybe.withDefault g.prices
                                    in
                                        { g | prices = toReplace }
                                )
                )
            |> Maybe.withDefault oldData


updateGames : WebSocketRefreshResult -> GameSources -> List GameEntry -> List GameEntry
updateGames newData sources oldData =
    newData.games
        |> Maybe.map
            (\( wishlist, owned ) ->
                case sources of
                    Owned ->
                        owned

                    WishList ->
                        wishlist

                    Both ->
                        List.append wishlist owned
            )
        |> Maybe.map (List.sortBy getName)
        |> Maybe.withDefault oldData


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
