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

type alias Model = {loadedUser : Maybe User, enteredUser : User, message : String}

serializeUser : User -> List (String, String)
serializeUser u =
    [("steamUsername", getSteamUserName u), ("steamIsAlternate", String.toLower <| toString u.steamAlternate), ("gogUsername", getGogUserName u)]

getSteamUserName : User -> String
getSteamUserName user = Maybe.withDefault "" user.username1

getGogUserName : User -> String
getGogUserName user = Maybe.withDefault "" user.username2

initialModel : Model
initialModel = Model Nothing (User Nothing (Just "") False (Just "")) ""

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
      ({model | message = ""}, serializeUser model.enteredUser |> fetchUser |> getResponse)
    CreateUpdate ->
      ({model | message = ""}, serializeUser model.enteredUser |> createUpdateUser |> getResponse)
    UserFetched u ->
      ({model | loadedUser = Just u, enteredUser = u, message = ""}, Cmd.none)
    SteamUsernameChange u ->
        let
            oldUser = model.enteredUser
            newUser = {oldUser | username1 = Just u}
        in
            ({model | enteredUser = newUser, message = ""}, Cmd.none)
    GogUsernameChange u ->
      let
          oldUser = model.enteredUser
          newUser = {oldUser | username2 = Just u}
      in
          ({model | enteredUser = newUser, message = ""}, Cmd.none)
    SteamAlternateChange c ->
        let
            oldUser = model.enteredUser
            newUser = {oldUser | steamAlternate = c}
        in
            ({model | enteredUser = newUser, message = ""}, Cmd.none)
    ResponseError err -> ({model | loadedUser = Nothing, message = toString err} , Cmd.none)


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
    let
        loadedSteamUsername = (Maybe.withDefault "" <| Maybe.map getSteamUserName model.loadedUser)
        loadedGogUsername = (Maybe.withDefault "" <| Maybe.map getGogUserName model.loadedUser)
        loadedSteamAlternate = (Maybe.withDefault "" <| Maybe.map (\u -> toString u.steamAlternate) model.loadedUser)
    in
        form [onSubmit FetchUser]
            [ span[][text model.message]
            , br[][]
            , label[][text "Steam username:", br[][], text loadedSteamUsername, br[][], input[type_ "text", name "username1", onInput SteamUsernameChange, value <| getSteamUserName model.enteredUser][]]
            , br[][]
            , label[][text "Alternate Steam login:", br[][], text loadedSteamAlternate, br[][], input [type_ "checkbox", name "Alternate", checked model.enteredUser.steamAlternate, onCheck SteamAlternateChange][]]
            , br[][]
            , label[][text "Gog username:", br[][], text loadedGogUsername, br[][], input[type_ "text", name "username2", onInput GogUsernameChange, value <| getGogUserName model.enteredUser][]]
            , input[type_ "submit", style [("display","none")]][]
            , br[][]
            ]

mainPageLink model =
    let
        userId = Maybe.withDefault 1 <| Maybe.andThen (\u -> u.id) model.loadedUser
    in
        Maybe.withDefault [] (Maybe.map (\_ ->
        [form [method "get", action Router.mainPageUrl]
        [ input [type_ "hidden", name "sources", value <| toString WishList][]
        , input [type_ "hidden", name "userId",  value <| toString userId][]
        , button[type_ "submit"][text "Continue"]]]) model.loadedUser)
