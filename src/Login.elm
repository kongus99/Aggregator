module Login exposing (..)
import Html exposing (Html, button, div, text, form, br, input)
import Html.Attributes exposing (action, type_, name, style, method)
import Html.Events exposing (onClick, onSubmit, onInput)
import Model exposing(..)
import Router exposing(fetchUser, createUpdateUser)
import Http

main =
  Html.program
    { init = (initialModel, Cmd.none)
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }

-- MODEL

type alias Model = {user : Maybe User, u1 : SteamUsername, u2 : GogUserName}

initialModel : Model
initialModel = Model Nothing "" ""

-- UPDATE

type Msg
  = FetchUser |
    CreateUpdate |
    UserFetched User |
    SteamUsernameChange SteamUsername |
    GogUsernameChange GogUserName |
    ResponseError Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchUser -> (model, getResponse <| fetchUser [("steamUsername", model.u1), ("gogUsername", model.u2)])
    CreateUpdate ->
      (model, Cmd.none)
    UserFetched u ->
      (model, Cmd.none)
    SteamUsernameChange u ->
      ({model | u1 = u}, Cmd.none)
    GogUsernameChange u ->
      ({model | u2 = u}, Cmd.none)
    ResponseError err ->
        ({model | user = Nothing} , Cmd.none)


getResponse : Http.Request User -> Cmd Msg
getResponse request =
    Http.send (Router.resolveResponse UserFetched ResponseError) request

-- VIEW

view : Model -> Html Msg
view model =
  div[] <| List.append
       [ usernameForm model
       , br[][]
       , button[type_ "button", onClick CreateUpdate][text "Create/Update"]
       , br[][]
       ] (mainPage model)


usernameForm model =
    form [onSubmit FetchUser]
        [ text "Steam username"
        , br[][]
        , input[type_ "text", name "username1", onInput SteamUsernameChange][]
        , br[][]
        , text "Gog username"
        , br[][]
        , input[type_ "text", name "username2", onInput GogUsernameChange][]
        , input[type_ "submit", style [("display","none")]][]
        , br[][]
        ]

mainPage model =
    Maybe.withDefault [] (Maybe.map (\_ -> [form [method "get", action"/"][button[type_ "submit"][text "Continue"]]]) model.user)
