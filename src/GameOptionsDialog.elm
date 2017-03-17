module GameOptionsDialog exposing (model, emptyModel, view, fetch, Model, Msg, update)

import Array exposing (Array)
import Dialog
import Html exposing (Attribute, Html, div, h4, input, label, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, name, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Http
import Json.Decode as Json
import Model exposing (GameOptions, GameQuery)
import Router exposing (routes)


-- MODEL


type alias Model =
    { message : Maybe String, gameOptions : Maybe GameOptions }


model : GameOptions -> Model
model options =
    Model Nothing (Just options)


emptyModel : Model
emptyModel =
    Model Nothing Nothing



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
    case (Array.get n a) of
        Nothing ->
            a

        Just element ->
            Array.set n (f element) a


updateQuery : Int -> (GameQuery -> GameQuery) -> Model -> Model
updateQuery queryIndex queryUpdate model =
    let
        updateQueries queries =
            updateArray queryIndex queryUpdate queries

        updateOptions options =
            { options | queries = updateQueries options.queries }
    in
        { model | gameOptions = Maybe.map updateOptions model.gameOptions }


serializeSelectedQuery : Int -> (GameQuery -> List ( String, String )) -> Model -> List ( String, String )
serializeSelectedQuery queryIndex querySerializer model =
    Maybe.andThen (\o -> Array.get queryIndex o.queries) model.gameOptions |> Maybe.map querySerializer |> Maybe.withDefault []


serializeSteamId : Model -> List ( String, String )
serializeSteamId model =
    model.gameOptions |> Maybe.map (\o -> [ ( "steamId", toString o.entry.steamId ) ]) |> Maybe.withDefault []


update : Int -> Msg -> Model -> ( Model, Cmd Msg )
update userId msg model =
    case msg of
        SwitchTo queryIndex newSelectedResult ->
            let
                updateResult res =
                    if res == Nothing then
                        Just newSelectedResult
                    else
                        Maybe.andThen
                            (\r ->
                                if (r == newSelectedResult) then
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
                ( { newModel | message = Nothing }, saveSwitched userId serialized newModel )

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
                ( { newModel | message = Nothing }, newResults userId serialized model )

        NewResults results ->
            ( { model | message = Nothing, gameOptions = Just results }, Cmd.none )


fetch : Int -> Maybe Int -> (GameOptions -> c) -> (Http.Error -> c) -> Cmd c
fetch userId steamId mess err =
    let
        send id =
            [ ( "userId", toString userId ), ( "steamId", toString id ) ]
                |> routes.gameOptions.fetch
                |> .request
                |> Http.send (Router.resolveResponse mess err)
    in
        Maybe.map send steamId |> Maybe.withDefault Cmd.none


saveSwitched userId serialized model =
    List.append (( "userId", toString userId ) :: serialized) (serializeSteamId model)
        |> routes.gameOptions.changeSelectedSearch
        |> .request
        |> Http.send (Router.resolveResponse Switched DialogError)


newResults userId serialized model =
    List.append (( "userId", toString userId ) :: serialized) (serializeSteamId model)
        |> routes.gameOptions.fetchSearchResults
        |> .request
        |> Http.send (Router.resolveResponse NewResults DialogError)



-- VIEW


view : (Msg -> msg) -> msg -> Model -> Html msg
view wrapper close model =
    Dialog.view <|
        Maybe.map
            (\o ->
                { closeMessage = Just close
                , containerClass = Just "game-options-class"
                , header = Just <| dialogHeader o
                , body = Just <| Html.map wrapper (dialogBody o)
                , footer = Maybe.map text model.message
                }
            )
            model.gameOptions


dialogHeader : GameOptions -> Html msg
dialogHeader options =
    h4 [] [ text options.entry.name ]


dialogBody : GameOptions -> Html Msg
dialogBody options =
    div []
        [ table [ class "table table-striped table-bordered" ]
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


tableRow : Int -> GameQuery -> Html Msg
tableRow index gameQuery =
    tr []
        [ th []
            [ text gameQuery.site ]
        , td []
            [ input [ type_ "text", value gameQuery.query, onEnter (GetNewResults index), onInput (ChangeQuery index) ] [] ]
        , td []
            (List.map
                (queryResult gameQuery.selectedResult (SwitchTo index) index)
                gameQuery.results
            )
        ]


queryResult : Maybe String -> (String -> Msg) -> Int -> String -> Html Msg
queryResult selectedResult msg index currentResult =
    div [ class "radio" ]
        [ label []
            [ input [ name <| "queryResult" ++ (toString index), type_ "radio", value currentResult, checked ((Maybe.withDefault "" selectedResult) == currentResult), onClick (msg currentResult) ]
                []
            , text currentResult
            ]
        ]


onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)
