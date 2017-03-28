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
        )

import Dict
import Model exposing (..)


type alias GameEntry =
    { gog : List GogEntry, steam : List SteamEntry, prices : List PriceEntry }


update : WebSocketRefreshResult -> List GameEntry -> List GameEntry
update newData oldData =
    oldData
        |> updateSteamEntries (newData.steamGames |> Maybe.withDefault [])



--    |> updateGogEntries (newData.steamGames |> Maybe.withDefault [])
--    |> updatePriceEntries (newData.steamGames |> Maybe.withDefault [])


updateSteamEntries : List SteamEntry -> List GameEntry -> List GameEntry
updateSteamEntries steamEntries gameEntries =
    let
        steamEntriesDict =
            steamEntries |> List.map (\e -> ( e.steamId, e )) |> Dict.fromList

        updateSteamEntry steamEntry =
            Dict.get steamEntry.steamId steamEntriesDict |> Maybe.withDefault steamEntry

        updateGameEntry gameEntry =
            { gameEntry | steam = gameEntry.steam |> List.map updateSteamEntry }
    in
        if List.isEmpty steamEntries then
            gameEntries
        else
            List.map updateGameEntry gameEntries


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
