module Login exposing (..)
import Html exposing (Html, button, div, text, form, br, input)
import Html.Attributes exposing (action, type_, name, style, method)
import Html.Events exposing (onClick, onSubmit, onInput)

main =
  Html.program
    { init = (initialModel, Cmd.none)
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }

-- MODEL

type alias SteamUsername = String

type alias GogUserName = String

type alias User = {username1 : SteamUsername, username2 : GogUserName, id : Int}

type alias Model = {user : Maybe User, u1 : SteamUsername, u2 : GogUserName}

initialModel : Model
initialModel = Model Nothing "" ""

-- UPDATE

type Msg
  = FetchUser |
    CreateUpdate |
    UserFetched User |
    SteamUsernameChange SteamUsername |
    GogUsernameChange GogUserName

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchUser ->
        let
            x = Debug.log "su" model.u1
            y = Debug.log "gu" model.u2
        in
            (model, Cmd.none)
    CreateUpdate ->
      (model, Cmd.none)
    UserFetched u ->
      (model, Cmd.none)
    SteamUsernameChange u ->
      ({model | u1 = u}, Cmd.none)
    GogUsernameChange u ->
      ({model | u2 = u}, Cmd.none)


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
