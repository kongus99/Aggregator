module GameEntry
    exposing
        ( GameEntry
        , update
        , WebSocketRefreshResult
        , GameEntryRow
        , toGameEntryRow
        , serializeValue
        )

import Dict exposing (Dict)
import Dict.Extra as Dicts
import Model exposing (..)
import Price exposing (AlternatePrice, Price, priceToString)
import Set exposing (Set)


type alias GameEntry =
    { gog : List GogEntry, steam : List SteamEntry, prices : List AlternatePrice }


type alias SerializableValue a =
    { value : a, serialize : a -> String }


type alias GameEntryRow =
    { steamId : Maybe Int
    , gameOn : Maybe GameOn
    , name : String
    , link : String
    , genres : SerializableValue (List String)
    , tags : SerializableValue (List String)
    , price : Maybe (SerializableValue Price)
    , alternatePrices : List AlternatePrice
    }


emptySerializableValue a =
    SerializableValue a (\_ -> "")


emptyGameRow =
    GameEntryRow Nothing Nothing "" "" (emptySerializableValue []) (emptySerializableValue []) Nothing []


type alias WishlistEntries =
    List GameEntry


type alias OwnedEntries =
    List GameEntry


type alias WebSocketRefreshResult =
    { games : Maybe ( WishlistEntries, OwnedEntries ), prices : Maybe ( List Int, List AlternatePrice ) }


serializeValue : SerializableValue a -> String
serializeValue v =
    v.serialize v.value


update : WebSocketRefreshResult -> GameSources -> List GameEntryRow -> List GameEntryRow
update newData sources oldData =
    updateGames newData sources oldData |> updatePrices newData sources


updatePrices : WebSocketRefreshResult -> GameSources -> List GameEntryRow -> List GameEntryRow
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
                                            g.steamId
                                                |> Maybe.andThen (\id -> Dict.get id groupedPrices)
                                                |> Maybe.withDefault g.alternatePrices
                                    in
                                        { g | alternatePrices = toReplace }
                                )
                )
            |> Maybe.withDefault oldData


updateGames : WebSocketRefreshResult -> GameSources -> List GameEntryRow -> List GameEntryRow
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
        |> Maybe.map (List.map toGameEntryRow)
        |> Maybe.map (List.sortBy .name)
        |> Maybe.withDefault oldData


toGameEntryRow : GameEntry -> GameEntryRow
toGameEntryRow gameEntry =
    let
        gameOn =
            if List.isEmpty gameEntry.gog then
                Just Steam
            else if List.isEmpty gameEntry.steam then
                Just Gog
            else
                Nothing

        steamToRow s =
            GameEntryRow
                (Just s.steamId)
                gameOn
                s.name
                s.link
                { value = s.genres, serialize = String.join ", " }
                { value = s.tags, serialize = String.join ", " }
                (Maybe.map (\pr -> { value = pr, serialize = priceToString }) s.price)
                gameEntry.prices

        gogToRow g =
            GameEntryRow
                Nothing
                gameOn
                g.title
                g.link
                { value = g.genres, serialize = String.join ", " }
                (emptySerializableValue [])
                (Maybe.map (\pr -> { value = pr, serialize = priceToString }) g.price)
                gameEntry.prices

        maybeSteamEntryRow list =
            List.head list |> Maybe.map steamToRow

        gogEntryRow list =
            List.head list |> Maybe.map gogToRow |> Maybe.withDefault emptyGameRow
    in
        maybeSteamEntryRow gameEntry.steam |> Maybe.withDefault (gogEntryRow gameEntry.gog)
