module GameOptionsDialog exposing (model, emptyModel, view, fetch, Model)

import Dialog
import Html exposing (Html, br, div, h2, h3, h4, input, label, option, p, select, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, checked, class, name, type_, value)
import Http
import Model exposing (GameOptions)
import Router


-- MODEL


type alias Model =
    { gameOptions : Maybe GameOptions }


model options =
    Model <| Just options


emptyModel =
    Model Nothing



-- UPDATE


fetch : Maybe Int -> (GameOptions -> c) -> (Http.Error -> c) -> Cmd c
fetch steamId mess err =
    let
        send id =
            Http.send (Router.resolveResponse mess err) <| Router.fetchGameOptions [ ( "gameId", toString id ) ]
    in
        Maybe.map send steamId |> Maybe.withDefault Cmd.none



-- VIEW


view : msg -> Model -> Html msg
view mess model =
    Dialog.view <|
        Maybe.map
            (\o ->
                { closeMessage = Just mess
                , containerClass = Just "your-container-class"
                , header = Just dialogHeader
                , body = Just (dialogBody o)
                , footer = Nothing
                }
            )
            model.gameOptions


dialogHeader =
    h3 [] [ text "Search Options" ]


dialogBody options =
    div []
        [ h4 [] [ text options.entry.name ]
        , table [ class "table table-striped table-bordered" ]
            [ tableHead
            , tbody [] (List.map tableRow options.queries)
            ]
        ]


tableHead =
    thead []
        [ tr []
            [ th []
                [ text "Site" ]
            , th []
                [ text "Query" ]
            , th []
                [ text "Results" ]
            ]
        ]


tableRow gameQuery =
    tr []
        [ th []
            [ text gameQuery.site ]
        , td []
            [ input [ type_ "text", value gameQuery.query ] [] ]
        , td []
            (List.indexedMap
                (queryResult gameQuery.selectedResult)
                gameQuery.results
            )
        ]


queryResult selectedResult index r =
    div [ class "radio" ]
        [ label []
            [ input [ name "selectedResult", type_ "radio", value r, checked (selectedResult == index) ]
                []
            , text r
            ]
        ]



--type alias GameQuery =
--    { query : String, site : String, results : List String }
--
--
--type alias GameOptions =
--    { entry : SteamEntry, queries : List GameQuery }
