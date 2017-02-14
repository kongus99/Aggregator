module GameOptionsDialog exposing (model, emptyModel, view, fetch, Model, Msg)

import Array
import Dialog
import Html exposing (Html, br, div, h2, h3, h4, input, label, option, p, select, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, checked, class, name, type_, value)
import Html.Events exposing (onClick)
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


type Msg
    = SwitchTo Int



--
--
--update : Msg -> Model -> Model
--update msg model =
--  case msg of
--    SwitchTo selectedResult ->
--      { model | fontSize = newFontSize }


fetch : Maybe Int -> (GameOptions -> c) -> (Http.Error -> c) -> Cmd c
fetch steamId mess err =
    let
        send id =
            Http.send (Router.resolveResponse mess err) <| Router.fetchGameOptions [ ( "gameId", toString id ) ]
    in
        Maybe.map send steamId |> Maybe.withDefault Cmd.none



-- VIEW


view : msg -> (Msg -> msg) -> Model -> Html msg
view close wrapper model =
    let
        x =
            Maybe.map
                (\o ->
                    { closeMessage = Just close
                    , containerClass = Just "your-container-class"
                    , header = Just dialogHeader
                    , body = Just <| Html.map wrapper (dialogBody o)
                    , footer = Nothing
                    }
                )
                model.gameOptions
    in
        Dialog.view x


dialogHeader =
    h3 [] [ text "Search Options" ]


dialogBody options =
    div []
        [ h4 [] [ text options.entry.name ]
        , table [ class "table table-striped table-bordered" ]
            [ tableHead
            , tbody [] (Array.map tableRow options.queries |> Array.toList)
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
            (Array.indexedMap
                (queryResult gameQuery.selectedResult SwitchTo)
                gameQuery.results
                |> Array.toList
            )
        ]


queryResult : Int -> (Int -> msg) -> Int -> String -> Html msg
queryResult selectedResult msg index currentResult =
    div [ class "radio" ]
        [ label []
            [ input [ name "selectedResult", type_ "radio", value currentResult, checked (selectedResult == index), onClick (msg index) ]
                []
            , text currentResult
            ]
        ]
