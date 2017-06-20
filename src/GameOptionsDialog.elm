module GameOptionsDialog exposing (Model, Msg, close, emptyModel, model, open, refresh, update, view)

import Array exposing (Array)
import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import Bootstrap.Table as Table
import Html exposing (Attribute, Html, div, h4, input, label, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, name, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import HtmlHelpers exposing (onEnter)
import Http
import Json.Decode as Json
import Model exposing (GameOptions, GameQuery)
import Router exposing (routes)


-- MODEL


type alias Model msg =
    { userId : Int, steamId : Int, message : Maybe String, gameOptions : Maybe GameOptions, state : Modal.State, success : Msg -> msg, error : Http.Error -> msg }


model : Int -> Int -> (Msg -> msg) -> (Http.Error -> msg) -> GameOptions -> Model msg
model userId steamId success error options =
    Model userId steamId Nothing (Just options) Modal.visibleState success error


emptyModel : Int -> Int -> (Msg -> msg) -> (Http.Error -> msg) -> Model msg
emptyModel userId steamId success error =
    Model userId steamId Nothing Nothing Modal.hiddenState success error



-- UPDATE


type Msg
    = SwitchTo Int String
    | Switched String
    | ChangeQuery Int String
    | GetNewResults Int
    | NewResults GameOptions
    | DialogError Http.Error


updateArray : Int -> (a -> a) -> Array a -> Array a
updateArray n f a =
    case Array.get n a of
        Nothing ->
            a

        Just element ->
            Array.set n (f element) a


updateQuery : Int -> (GameQuery -> GameQuery) -> Model msg -> Model msg
updateQuery queryIndex queryUpdate model =
    let
        updateQueries queries =
            updateArray queryIndex queryUpdate queries

        updateOptions options =
            { options | queries = updateQueries options.queries }
    in
    { model | gameOptions = Maybe.map updateOptions model.gameOptions }


serializeSelectedQuery : Int -> (GameQuery -> List ( String, String )) -> Model msg -> List ( String, String )
serializeSelectedQuery queryIndex querySerializer model =
    Maybe.andThen (\o -> Array.get queryIndex o.queries) model.gameOptions |> Maybe.map querySerializer |> Maybe.withDefault []


update : Msg -> Model msg -> ( Model msg, Cmd Msg )
update msg model =
    case msg of
        SwitchTo queryIndex newSelectedResult ->
            let
                updateResult res =
                    if res == Nothing then
                        Just newSelectedResult
                    else
                        Maybe.andThen
                            (\r ->
                                if r == newSelectedResult then
                                    Nothing
                                else
                                    Just newSelectedResult
                            )
                            res

                newModel =
                    updateQuery queryIndex (\q -> { q | selectedResult = updateResult q.selectedResult }) model

                getSelectedResult res =
                    Maybe.map (\r -> [ ( "selectedResult", r ) ]) res |> Maybe.withDefault []

                serialized =
                    serializeSelectedQuery queryIndex (\q -> ( "site", q.site ) :: getSelectedResult q.selectedResult) newModel
            in
            ( { newModel | message = Nothing }, saveSwitched serialized newModel )

        Switched msg ->
            ( { model | message = Just msg }, Cmd.none )

        DialogError err ->
            ( { model | message = Just <| toString err }, Cmd.none )

        ChangeQuery queryIndex newQuery ->
            let
                newModel =
                    updateQuery queryIndex (\q -> { q | query = newQuery }) model
            in
            ( { newModel | message = Nothing }, Cmd.none )

        GetNewResults queryIndex ->
            let
                newModel =
                    updateQuery queryIndex (\q -> { q | results = [] }) model

                serialized =
                    serializeSelectedQuery queryIndex (\q -> [ ( "query", q.query ), ( "site", q.site ) ]) newModel
            in
            ( { newModel | message = Nothing }, newResults serialized model )

        NewResults results ->
            ( { model | message = Nothing, gameOptions = Just results, state = Modal.visibleState }, Cmd.none )


open : Model msg -> Cmd msg
open model =
    [ ( "userId", toString model.userId ), ( "steamId", toString model.steamId ) ]
        |> routes.gameOptions.fetch
        |> .request
        |> Http.send (Router.resolveResponse (\r -> model.success (NewResults r)) model.error)


close : Model msg -> Model msg
close model =
    { model | state = Modal.hiddenState }


refresh : (String -> msg) -> Model msg -> Cmd msg
refresh msg model =
    [ ( "userId", toString model.userId ), ( "steamId", toString model.steamId ) ]
        |> routes.gameOptions.triggerRefresh
        |> .request
        |> Http.send (Router.resolveResponse msg model.error)


saveSwitched serialized model =
    ( "userId", toString model.userId )
        :: (( "steamId", toString model.steamId ) :: serialized)
        |> routes.gameOptions.changeSelectedSearch
        |> .request
        |> Http.send (Router.resolveResponse Switched DialogError)


newResults serialized model =
    ( "userId", toString model.userId )
        :: (( "steamId", toString model.steamId ) :: serialized)
        |> routes.gameOptions.fetchSearchResults
        |> .request
        |> Http.send (Router.resolveResponse NewResults DialogError)



-- VIEW


view : (Modal.State -> msg) -> Model msg -> Html msg
view close model =
    Maybe.map
        (\o ->
            Modal.config close
                |> Modal.large
                |> dialogHeader o
                |> dialogBody model.success o
                |> Modal.footer [] [ text (Maybe.withDefault "" model.message) ]
                |> Modal.view model.state
        )
        model.gameOptions
        |> Maybe.withDefault (div [] [])


dialogHeader : GameOptions -> Modal.Config msg -> Modal.Config msg
dialogHeader options config =
    Modal.h4 [] [ text options.entry.name ] config


dialogBody : (Msg -> msg) -> GameOptions -> Modal.Config msg -> Modal.Config msg
dialogBody wrapper options config =
    Modal.body []
        [ Table.table
            { options = [ Table.striped, Table.bordered ]
            , thead = tableHead
            , tbody = Table.tbody [] (Array.indexedMap (tableRow wrapper) options.queries |> Array.toList)
            }
        ]
        config


tableHead =
    Table.thead []
        [ Table.tr []
            [ Table.th []
                [ text "Site" ]
            , Table.th []
                [ text "Query" ]
            , Table.th []
                [ text "Results" ]
            ]
        ]


tableRow : (Msg -> msg) -> Int -> GameQuery -> Table.Row msg
tableRow wrapper index gameQuery =
    Table.tr []
        [ Table.th []
            [ text gameQuery.site ]
        , Table.td []
            [ input [ type_ "text", value gameQuery.query, onEnter (GetNewResults index), onInput (ChangeQuery index) ] [] |> Html.map wrapper ]
        , Table.td []
            (List.map
                (\q -> queryResult gameQuery.selectedResult (SwitchTo index) index q |> Html.map wrapper)
                gameQuery.results
            )
        ]


queryResult : Maybe String -> (String -> Msg) -> Int -> String -> Html Msg
queryResult selectedResult msg index currentResult =
    div [ class "radio" ]
        [ label []
            [ input [ name <| "queryResult" ++ toString index, type_ "radio", value currentResult, checked (Maybe.withDefault "" selectedResult == currentResult), onClick (msg currentResult) ]
                []
            , text currentResult
            ]
        ]
