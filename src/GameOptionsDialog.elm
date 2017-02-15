module GameOptionsDialog exposing (model, emptyModel, view, fetch, Model, Msg, update)

import Array exposing (Array)
import Dialog
import Html exposing (Html, br, div, h2, h3, h4, input, label, option, p, select, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, checked, class, name, type_, value)
import Html.Events exposing (onClick)
import Http
import Model exposing (GameOptions, GameQuery)
import Process
import Router
import Task
import Time exposing (second)


-- MODEL


type alias Model msg =
    { message : Maybe String, closeMsg : msg, wrapper : Msg -> msg, gameOptions : Maybe GameOptions }


model : msg -> (Msg -> msg) -> GameOptions -> Model msg
model closeMsg wrapper options =
    Model Nothing closeMsg wrapper (Just options)


emptyModel : msg -> (Msg -> msg) -> Model msg
emptyModel closeMsg wrapper =
    Model Nothing closeMsg wrapper Nothing



-- UPDATE


type Msg
    = SwitchTo Int Int
    | Switched String
    | DialogError Http.Error


updateArray : Int -> (a -> a) -> Array a -> Array a
updateArray n f a =
    case (Array.get n a) of
        Nothing ->
            a

        Just element ->
            Array.set n (f element) a


update : Int -> Msg -> Model msg -> ( Model msg, Cmd msg )
update userId msg model =
    case msg of
        SwitchTo queryIndex resultIndex ->
            let
                updateQueries queries =
                    updateArray queryIndex (\q -> { q | selectedResult = resultIndex }) queries

                updateOptions options =
                    { options | queries = updateQueries options.queries }
            in
                ( { model | gameOptions = Maybe.map updateOptions model.gameOptions, message = Nothing }, saveSwitched userId ( queryIndex, resultIndex ) model )

        Switched msg ->
            ( { model | message = Just msg }, Cmd.none )

        DialogError err ->
            ( { model | message = Just <| toString err }, Cmd.none )


fetch : Maybe Int -> (GameOptions -> c) -> (Http.Error -> c) -> Cmd c
fetch steamId mess err =
    let
        send id =
            Http.send (Router.resolveResponse mess err) <| Router.fetchGameOptions [ ( "gameId", toString id ) ]
    in
        Maybe.map send steamId |> Maybe.withDefault Cmd.none


saveSwitched userId ( queryIndex, resultIndex ) model =
    let
        send =
            Http.send (Router.resolveResponse Switched DialogError) <| Router.saveSelectedSearchResult [ ( "userId", toString userId ) ]
    in
        Cmd.map model.wrapper send



-- VIEW


view : Model msg -> Html msg
view model =
    Dialog.view <|
        Maybe.map
            (\o ->
                { closeMessage = Just model.closeMsg
                , containerClass = Just "your-container-class"
                , header = Just dialogHeader
                , body = Just <| Html.map model.wrapper (dialogBody o)
                , footer = Maybe.map text model.message
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
            , tbody [] (Array.indexedMap tableRow options.queries |> Array.toList)
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


tableRow index gameQuery =
    tr []
        [ th []
            [ text gameQuery.site ]
        , td []
            [ input [ type_ "text", value gameQuery.query ] [] ]
        , td []
            (Array.indexedMap
                (queryResult gameQuery.selectedResult (SwitchTo index))
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
