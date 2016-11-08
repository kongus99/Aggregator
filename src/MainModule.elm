module MainModule exposing (..)
import Html exposing (Html, button, div, text, span, table, tr, th, td)
import Html.Attributes exposing(class)
import Html.App as App
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing (..)
import Task

main =
    App.program { init = ( initialModel, Cmd.none ), view = view, update = update, subscriptions = \_ -> Sub.none }

-- MODEL

type EntryConvergence = Gog | Steam | Both

type alias GameEntry = {name : String, convergence : EntryConvergence}

type alias Model = {entries : List GameEntry, message : String}

initialModel : Model
initialModel = {entries = [], message = "Click to refresh"}

-- UPDATE

type Msg
  = SendRefresh String
  | ReceiveRefresh (List GameEntry)
  | RefreshError Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SendRefresh address ->
      (model, getResponse address)
    ReceiveRefresh entries ->
      ({model | entries = entries} , Cmd.none)
    RefreshError err ->
      ({model | entries = [], message = toString err} , Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div [] <|
    [ button [ onClick <| SendRefresh "gogData"] [ text "Fetch from gog"   ]
    , button [ onClick <| SendRefresh "steamData"] [ text "Fetch from steam" ]
    , div [] [ text (toString model.message) ]
    , table[] <| gameTableTitle :: (List.map gameTableRow model.entries)
    ]

gameTableTitle =
    tr [] [ th[][text "Game"]
          , th[][text "Gog/Steam/Both"]
          ]

gameTableRow e =
    tr [] [ td[][text e.name]
          , td[class <| "cell_" ++ toString e.convergence  ][]
          ]

getResponse address=
  let
    url = "http://localhost:9000/" ++ address
  in
    Task.perform RefreshError ReceiveRefresh (Http.get decodeResponse url)

decodeResponse : Json.Decoder (List GameEntry)
decodeResponse =
  list (object2 GameEntry
                ("name" := string)
                (map convergenceFormString ("convergence" := string))
                )

convergenceFormString s =
    case s of
        "Gog" -> Gog
        "Steam" -> Steam
        _ -> Both
