module Login exposing (..)
import Html exposing (Html, button, div, text, form, br, input)
import Html.Attributes exposing (action, type_, name, style, method, value)
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

getSteamUserName : Model -> User -> String
getSteamUserName model user = Maybe.withDefault model.u1 user.username1

getGogUserName : Model -> User -> String
getGogUserName model user = Maybe.withDefault model.u2 user.username2

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
    FetchUser ->
      (model, getResponse <| fetchUser [("steamUsername", model.u1), ("gogUsername", model.u2)])
    CreateUpdate ->
      (model, getResponse <| createUpdateUser [("steamUsername", model.u1), ("gogUsername", model.u2)])
    UserFetched u ->
      ({model | user = Just u, u1 = getSteamUserName model u, u2 = getGogUserName model u}, Cmd.none)
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
        , input[type_ "text", name "username1", onInput SteamUsernameChange, value model.u1][]
        , br[][]
        , text "Gog username"
        , br[][]
        , input[type_ "text", name "username2", onInput GogUsernameChange, value model.u2][]
        , input[type_ "submit", style [("display","none")]][]
        , br[][]
        ]

mainPage model =
    Maybe.withDefault [] (Maybe.map (\_ -> [form [method "get", action"/"][button[type_ "submit"][text "Continue"]]]) model.user)
