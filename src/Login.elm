module Login exposing (..)
import Html exposing (Html, button, div, text, form, br, input, span, label)
import Html.Attributes exposing (action, type_, name, style, method, value, checked)
import Html.Events exposing (onClick, onSubmit, onInput, onCheck)
import Model exposing(..)
import Router exposing(fetchUser, createUpdateUser, mainPageUrl, changeSteamAlternate)
import Http

main =
  Html.program
    { init = (initialModel, Cmd.none)
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }

-- MODEL

type alias Model = {user : Maybe User, u1 : SteamUsername, u1Alternate: Bool, u2 : GogUserName, message : String}

getSteamUserName : Model -> User -> String
getSteamUserName model user = Maybe.withDefault model.u1 user.username1

getGogUserName : Model -> User -> String
getGogUserName model user = Maybe.withDefault model.u2 user.username2

initialModel : Model
initialModel = Model Nothing "" False "" ""

-- UPDATE

type Msg
  = FetchUser |
    CreateUpdate |
    UserFetched User |
    SteamUsernameChange SteamUsername |
    GogUsernameChange GogUserName |
    SteamAlternateChange Bool|
    ResponseError Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchUser ->
      ({model | message = ""}, getResponse <| fetchUser [("steamUsername", model.u1), ("gogUsername", model.u2)])
    CreateUpdate ->
      ({model | message = ""}, getResponse <| createUpdateUser [("steamUsername", model.u1), ("steamIsAlternate", String.toLower <| toString model.u1Alternate), ("gogUsername", model.u2)])
    UserFetched u ->
      ({model | user = Just u, u1 = getSteamUserName model u, u2 = getGogUserName model u, message = ""}, Cmd.none)
    SteamUsernameChange u ->
      ({model | u1 = u, message = ""}, Cmd.none)
    GogUsernameChange u ->
      ({model | u2 = u, message = ""}, Cmd.none)
    SteamAlternateChange c ->
        let
            alterValue = ("steamAlternate", String.toLower <| toString model.u1Alternate)
            maybeCmd = Maybe.andThen (\u -> Maybe.map toString u.id) model.user |> Maybe.map (\id -> getResponse <| changeSteamAlternate [("steamId", id), alterValue])
        in
            ({model | message = "", u1Alternate = c}, Maybe.withDefault Cmd.none maybeCmd)
    ResponseError err -> ({model | user = Nothing, message = toString err} , Cmd.none)


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
       ] (mainPageLink model)


usernameForm model =
    form [onSubmit FetchUser]
        [ span[][text model.message]
        , br[][]
        , text "Steam username"
        , br[][]
        , input[type_ "text", name "username1", onInput SteamUsernameChange, value model.u1][]
        , label[][input [type_ "checkbox", name "Alternate", checked model.u1Alternate, onCheck SteamAlternateChange][], text "Alternate Steam Login?"]
        , br[][]
        , text "Gog username"
        , br[][]
        , input[type_ "text", name "username2", onInput GogUsernameChange, value model.u2][]
        , input[type_ "submit", style [("display","none")]][]
        , br[][]
        ]

mainPageLink model =
    let
        userId = Maybe.withDefault 1 <| Maybe.andThen (\u -> u.id) model.user
    in
        Maybe.withDefault [] (Maybe.map (\_ ->
        [form [method "get", action Router.mainPageUrl]
        [ input [type_ "hidden", name "sources", value <| toString WishList][]
        , input [type_ "hidden", name "userId",  value <| toString userId][]
        , button[type_ "submit"][text "Continue"]]]) model.user)
