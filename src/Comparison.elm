module Comparison exposing (..)
import AllDict exposing (AllDict)
import Html exposing (Html, button, div, text, span, table, tr, th, td, select, option)
import Html.Attributes exposing(class, selected, value)
import Html.App as App
import Html.Events exposing (onClick, on, targetValue)
import Http
import Json.Decode as Json exposing (tuple2, int, string, list, object3, (:=))
import Task
import String
import Model exposing (..)

main =
    App.program { init = ( initialModel, refresh initialModel),
    view = view, update = update, subscriptions = \_ -> Sub.none }

-- MODEL
type PageHalf = Left | Right

type alias NamedEntry = {internalId : Int, externalId : Int, name : String}

type alias Model = {half : AllDict PageHalf (GameOn, List NamedEntry) String, message : String}

refresh model =
    getResponse "comparison/data" [(toUrl Left, getOn Left model), (toUrl Right, getOn Right model)]

initialModel = {half = AllDict.fromList toString [(Left, (Gog, [])), (Right, (Steam, []))], message = ""}

toUrl half =
    case half of
        Left -> "left"
        Right -> "right"

getHalf half model =
    AllDict.get half model.half |> Maybe.withDefault (Gog, [])

getOn half model =
    getHalf half model |> fst

getEntries half model =
   getHalf half model |> snd

-- UPDATE

type Msg
  = ReceiveData (List NamedEntry, List NamedEntry)
  | DataError Http.Error
  | Refresh PageHalf String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceiveData (leftList, rightList) ->
        let
            oldLeftOn = getOn Left model
            oldRightOn = getOn Right model
            newHalves = AllDict.insert Left (oldLeftOn, leftList) model.half |> AllDict.insert Right (oldRightOn, rightList)
        in
            ({model | half = newHalves} , Cmd.none)
    DataError err ->
      ({model | half = initialModel.half, message = toString err} , Cmd.none)
    Refresh half value ->
        let
            gameOn = case value of
                "Steam" -> Steam
                _ -> Gog
            newHalves = AllDict.insert half (gameOn, getEntries half model) model.half
            newModel = { model | half =  newHalves}
        in
            (newModel, refresh newModel)

-- VIEW

view : Model -> Html Msg
view model =
  div [] <|
    [ div [] (if String.isEmpty model.message then [] else [ text (toString model.message) ])
    , div [] [ table[class <| "inlineTable"] <| selectedSource Left model  :: title Left model  :: (List.map tableRow (getEntries Left model))
             , table[class <| "inlineTable"] <| selectedSource Right model :: title Right model :: (List.map tableRow (getEntries Right model))
             ]
    ]

selectedSource half model =
    let
        gameOn = getOn half model
    in
        select [onSelect <| Refresh half] [option [selected (gameOn == Gog), value <| toString Gog][text <| toString Gog], option [selected (gameOn == Steam), value <| toString Steam][text <| toString Steam]]

tableRow e = tr [] [ td[][text e.name] ]

title half model =
    let
        gameOn = getOn half model
        tableTitle title = tr [] [ th[][text title] ]
    in
        case gameOn of
            Gog -> tableTitle "Gog Game"
            Steam -> tableTitle "Steam Game"

getResponse address params =
  let
    url = "http://localhost:9000/" ++ address ++ "?" ++ (joinParameters params)
  in
    Task.perform DataError ReceiveData (Http.get decodeResponse url)

joinParameters params =
    String.join "&&" (List.map (\((k, v)) -> k ++ "=" ++ (toString v)) params)

onSelect : (String -> Msg) -> Html.Attribute Msg
onSelect msg =
    on "change" (Json.map msg targetValue)

decodeResponse : Json.Decoder (List NamedEntry, List NamedEntry)
decodeResponse =
  tuple2
    (\a -> \b -> (a, b))
    (list <| object3 NamedEntry ("internalId" := int) ("externalId" := int) ("name" := string))
    (list <| object3 NamedEntry ("internalId" := int) ("externalId" := int) ("name" := string))
