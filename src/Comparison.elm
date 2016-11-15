module Comparison exposing (..)
import AllDict exposing (AllDict)
import Html exposing (Html, button, div, text, span, table, tr, th, td, select, option)
import Html.Attributes exposing(class, selected)
import Html.App as App
import Html.Events exposing (onClick, on)
import Http
import Json.Decode as Json exposing (..)
import Task
import String
import Model exposing (..)

main =
    App.program { init = ( initialModel, getResponse "comparison/data" [(toUrl Left, getOn Left initialModel), (toUrl Right, getOn Right initialModel)]),
    view = view, update = update, subscriptions = \_ -> Sub.none }

-- MODEL
type PageHalf = Left | Right

type alias NamedEntry = {internalId : Int, externalId : Int, name : String}

type alias Model = {half : AllDict PageHalf (GameOn, List NamedEntry) String, message : String}

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
--  | Refresh

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

-- VIEW

view : Model -> Html Msg
view model =
  div [] <|
    [ div [] (if String.isEmpty model.message then [] else [ text (toString model.message) ])
    , div [] [ table[class <| "inlineTable"] <| (selectedSource <| getOn Left model)  :: title (getOn Left model) tableTitle  :: (List.map tableRow (getEntries Left model))
             , table[class <| "inlineTable"] <| (selectedSource <| getOn Right model) :: title (getOn Right model) tableTitle :: (List.map tableRow (getEntries Right model))
             ]
    ]

selectedSource on =
    select [] [option [selected (on == Gog)][text <| toString Gog], option [selected (on == Steam)][text <| toString Steam]]

tableTitle title = tr [] [ th[][text title] ]

tableRow e = tr [] [ td[][text e.name] ]

title on fn =
    case on of
        Gog -> fn "Gog Game"
        Steam -> fn "Steam Game"

getResponse address params =
  let
    url = "http://localhost:9000/" ++ address ++ "?" ++ (joinParameters params)
  in
    Task.perform DataError ReceiveData (Http.get decodeResponse url)

joinParameters params =
    String.join "&&" (List.map (\((k, v)) -> k ++ "=" ++ (toString v)) params)

--onSelect : GameOn -> Html.Attribute msg
--onSelect msg =
--    on "change" (Json.succeed msg)

decodeResponse : Json.Decoder (List NamedEntry, List NamedEntry)
decodeResponse =
  tuple2
    (\a -> \b -> (a, b))
    (list <| object3 NamedEntry ("internalId" := int) ("externalId" := int) ("name" := string))
    (list <| object3 NamedEntry ("internalId" := int) ("externalId" := int) ("name" := string))
