module Login exposing (..)

import Array exposing (Array)
import Autocomplete
import Dom
import Html exposing (Html, button, div, text, form, br, input, span, label)
import Html.Attributes exposing (action, checked, class, classList, disabled, id, method, name, style, type_, value)
import Html.Events exposing (keyCode, onBlur, onCheck, onClick, onFocus, onInput, onSubmit, onWithOptions)
import Model exposing (..)
import Router exposing (routes, mainPageUrl)
import Http
import Json.Decode as Json
import Task


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SetAutoState Autocomplete.subscription



-- MODEL


type alias LoginData =
    { loadedUser : Maybe User, typedUser : User, selectedUser : Int, possibleUsers : Array User }


type alias Model =
    { data : LoginData, message : String, autoState : Autocomplete.State, activeInput : Maybe GameOn, showHowMany : Int }


serializeUser : User -> List ( String, String )
serializeUser u =
    [ ( "steamUsername", u.steamUsername ), ( "steamAlternate", toString u.steamAlternate |> String.toLower |> Just ), ( "gogUsername", u.gogUsername ), ( "userId", Maybe.map toString u.id ) ]
        |> List.filter (\( p1, p2 ) -> not (p2 == Nothing))
        |> List.map (\( p1, p2 ) -> ( p1, Maybe.withDefault "" p2 ))


emptyUser : User
emptyUser =
    User Nothing (Just "") False (Just "")


updateLoginData : (LoginData -> LoginData) -> Model -> Model
updateLoginData updater model =
    { model | data = updater model.data }


initialModel : Model
initialModel =
    Model (LoginData Nothing emptyUser -1 Array.empty) "" Autocomplete.empty Nothing 5


idGetter : Maybe GameOn -> User -> String
idGetter activeInput user =
    activeInput
        |> Maybe.andThen
            (\i ->
                if i == Steam then
                    user.steamUsername
                else
                    user.gogUsername
            )
        |> Maybe.withDefault ""


updateConfig : Maybe GameOn -> Autocomplete.UpdateConfig Msg User
updateConfig activeInput =
    Autocomplete.updateConfig
        { toId = (idGetter activeInput)
        , onKeyDown =
            \code maybeId ->
                if code == 38 || code == 40 then
                    Maybe.map PreviewUser maybeId
                else if code == 13 then
                    Maybe.map SelectUser maybeId
                else
                    Nothing
        , onTooLow = Nothing
        , onTooHigh = Nothing
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SelectUser id
        , separateSelections = False
        }



-- UPDATE


type Msg
    = SelectUser String
    | PreviewUser String
    | SetAutoState Autocomplete.Msg
    | CreateUpdateUser User
    | UserFetched User
    | UsersFetched (List User)
    | SteamChange SteamUsername
    | GogChange GogUserName
    | SteamGainFocus
    | GogGainFocus
    | FocusLost
    | SteamAlternateChange Bool
    | ResponseError Http.Error
    | NoOp
    | Wrap Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Wrap toTop ->
            ( model, Cmd.none )

        SelectUser name ->
            let
                y =
                    Debug.log "yyy" name

                index =
                    model.data.possibleUsers |> Array.toIndexedList |> List.filter (\( i, u ) -> idGetter model.activeInput u == name) |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault -1

                maybeUser =
                    model.data.possibleUsers |> Array.get index |> Debug.log "xxx"
            in
                ({ model | message = "", activeInput = Nothing } |> updateLoginData (\data -> { data | loadedUser = maybeUser, typedUser = maybeUser |> Maybe.withDefault data.typedUser, selectedUser = index }))
                    ! []

        PreviewUser name ->
            let
                index =
                    model.data.possibleUsers |> Array.toIndexedList |> List.filter (\( i, u ) -> idGetter model.activeInput u == name) |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault -1
            in
                ({ model | message = "" } |> updateLoginData (\data -> { data | selectedUser = index })) ! []

        SetAutoState autoMsg ->
            let
                ( newState, maybeMsg ) =
                    Autocomplete.update (updateConfig model.activeInput) autoMsg model.showHowMany model.autoState (Array.toList model.data.possibleUsers)

                newModel =
                    { model | autoState = newState }
            in
                case maybeMsg of
                    Nothing ->
                        newModel ! []

                    Just updateMsg ->
                        update updateMsg newModel

        CreateUpdateUser u ->
            { model | message = "" } ! [ serializeUser u |> routes.login.createUpdate |> (getResponse UserFetched) ]

        UserFetched u ->
            ({ model | message = "" } |> updateLoginData (\data -> { data | loadedUser = Just u, typedUser = u, selectedUser = -1, possibleUsers = Array.fromList [] })) ! []

        UsersFetched users ->
            ({ model | message = "" } |> updateLoginData (\data -> { data | selectedUser = 0, possibleUsers = Array.fromList users })) ! []

        SteamChange u ->
            let
                newModel =
                    { model | message = "" } |> updateLoginData (\data -> { data | typedUser = (\user -> { user | steamUsername = Just u }) data.typedUser })
            in
                newModel ! [ sendUsersRequest routes.login.fetchSteam newModel.data.typedUser u ]

        GogChange u ->
            let
                newModel =
                    { model | message = "" } |> updateLoginData (\data -> { data | typedUser = (\user -> { user | gogUsername = Just u }) data.typedUser })
            in
                newModel ! [ sendUsersRequest routes.login.fetchGog newModel.data.typedUser u ]

        SteamGainFocus ->
            { model
                | message = ""
                , activeInput =
                    if model.data.loadedUser == Nothing then
                        Just Steam
                    else
                        Nothing
            }
                ! [ sendUsersRequest routes.login.fetchSteam model.data.typedUser (Maybe.withDefault "" model.data.typedUser.steamUsername) ]

        GogGainFocus ->
            { model
                | message = ""
                , activeInput =
                    if model.data.loadedUser == Nothing then
                        Just Gog
                    else
                        Nothing
            }
                ! [ sendUsersRequest routes.login.fetchGog model.data.typedUser (Maybe.withDefault "" model.data.typedUser.gogUsername) ]

        FocusLost ->
            { model | message = "", activeInput = Nothing } ! []

        SteamAlternateChange c ->
            let
                oldUser =
                    model.data.typedUser

                newUser =
                    { oldUser | steamAlternate = c }

                changeAlternate args =
                    List.map
                        (\( p1, p2 ) ->
                            if p1 == "steamAlternate" then
                                ( p1, c |> toString |> String.toLower )
                            else
                                ( p1, p2 )
                        )
                        args

                sendUpdate u =
                    serializeUser u |> changeAlternate |> routes.login.steamAlternate |> (getResponse UserFetched)

                newModel =
                    { model | message = "" } |> updateLoginData (\data -> { data | typedUser = newUser })
            in
                newModel ! [ Maybe.map sendUpdate newModel.data.loadedUser |> Maybe.withDefault Cmd.none ]

        ResponseError err ->
            { model | message = toString err } ! []

        NoOp ->
            model ! []


sendUsersRequest method user username =
    if String.length username == 2 then
        serializeUser user |> method |> (getResponse UsersFetched)
    else
        Cmd.none


getResponse : (a -> Msg) -> Http.Request a -> Cmd Msg
getResponse msg request =
    Http.send (Router.resolveResponse msg ResponseError) request



-- VIEW


viewConfig : Maybe GameOn -> Autocomplete.ViewConfig User
viewConfig activeInput =
    let
        customizedLi keySelected mouseSelected user =
            { attributes =
                [ classList [ ( "autocomplete-item", True ), ( "key-selected", keySelected || mouseSelected ) ]
                , id (idGetter activeInput user)
                ]
            , children = [ Html.text (idGetter activeInput user) ]
            }
    in
        Autocomplete.viewConfig
            { toId = (idGetter activeInput)
            , ul = [ class "autocomplete-list" ]
            , li = customizedLi
            }


view : Model -> Html Msg
view model =
    div [ class "center" ] <| List.concat [ usernameForm model, createUpdateButton model, mainPageLink model ]


createUpdateButton model =
    model.data.loadedUser |> Maybe.map (\_ -> [ form [] [ button [ type_ "button", onClick <| CreateUpdateUser model.data.typedUser ] [ text "Create/Update" ], br [] [] ] ]) |> Maybe.withDefault []


usernameForm model =
    let
        userData maybeUser =
            ( maybeUser |> Maybe.andThen .steamUsername |> Maybe.withDefault "", maybeUser |> Maybe.andThen .gogUsername |> Maybe.withDefault "", maybeUser |> Maybe.map .steamAlternate |> Maybe.withDefault False )

        ( loadedSteamUsername, loadedGogUsername, loadedSteamAlternate ) =
            userData model.data.loadedUser

        ( typedSteamUsername, typedGogUsername, typedSteamAlternate ) =
            userData (Just model.data.typedUser)
    in
        [ form [ onSubmit NoOp ]
            [ span [] [ text model.message ]
            , steamInput model loadedSteamUsername typedSteamUsername
            , alternateSteamInput (model.data.loadedUser == Nothing) loadedSteamAlternate typedSteamAlternate
            , gogInput model loadedGogUsername typedGogUsername
            , div [ class "form-group" ] [ input [ type_ "submit", style [ ( "display", "none" ) ] ] [] ]
            ]
        , br [] []
        ]


alternateSteamInput dis loaded typed =
    div [ class "form-group" ]
        [ label []
            [ text "Alternate Steam login:"
            , br [] []
            , text <| toString loaded
            , br [] []
            , input
                [ type_ "checkbox"
                , disabled dis
                , checked typed
                , onCheck SteamAlternateChange
                ]
                []
            ]
        ]


steamInput model loaded typed =
    usernameInput model (model.activeInput == Just Steam) "Steam username:" SteamChange SteamGainFocus loaded typed


gogInput model loaded typed =
    usernameInput model (model.activeInput == Just Gog) "Gog username:" GogChange GogGainFocus loaded typed


usernameInput model show name inputMsg focusMsg loaded typed =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        dec =
            (Json.map
                (\code ->
                    if code == 38 || code == 40 then
                        Ok NoOp
                    else
                        Err "not handling that key"
                )
                keyCode
            )
                |> Json.andThen
                    fromResult

        fromResult : Result String a -> Json.Decoder a
        fromResult result =
            case result of
                Ok val ->
                    Json.succeed val

                Err reason ->
                    Json.fail reason
    in
        div [ class "form-group" ]
            [ label []
                [ text name
                , br [] []
                , text loaded
                , br [] []
                , input
                    [ type_ "text"
                    , disabled (not <| model.data.loadedUser == Nothing)
                    , id <| inputId focusMsg
                    , onInput inputMsg
                    , onFocus focusMsg
                    , value typed
                    , onWithOptions "keydown" options dec
                    ]
                    []
                , if show then
                    Html.map SetAutoState (Autocomplete.view (viewConfig model.activeInput) model.showHowMany model.autoState (Array.toList model.data.possibleUsers))
                  else
                    div [] []
                ]
            ]


inputId focusMsg =
    "input_ " ++ toString focusMsg


mainPageLink model =
    let
        userId =
            Maybe.withDefault 1 <| Maybe.andThen (\u -> u.id) model.data.loadedUser
    in
        Maybe.withDefault []
            (Maybe.map
                (\_ ->
                    [ form [ method "get", Router.mainPageUrl [] |> action ]
                        [ input [ type_ "hidden", name "sources", value <| toString WishList ] []
                        , input [ type_ "hidden", name "userId", value <| toString userId ] []
                        , button [ type_ "submit" ] [ text "Continue" ]
                        ]
                    ]
                )
                model.data.loadedUser
            )
