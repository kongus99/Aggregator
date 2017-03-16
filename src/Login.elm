module Login exposing (..)

import Array exposing (Array)
import Autocomplete
import Dom
import Html exposing (Html, button, div, text, form, br, input, span, label)
import Html.Attributes exposing (action, checked, class, classList, disabled, id, method, name, style, type_, value)
import Html.Events exposing (keyCode, onBlur, onCheck, onClick, onFocus, onInput, onSubmit, onWithOptions)
import LoginData exposing (LoginData)
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


type alias Model =
    { data : LoginData, message : String, autoState : Autocomplete.State, showHowMany : Int }


serializeUser : User -> List ( String, String )
serializeUser u =
    [ ( "steamUsername", u.steamUsername ), ( "steamAlternate", toString u.steamAlternate |> String.toLower |> Just ), ( "gogUsername", u.gogUsername ), ( "userId", Maybe.map toString u.id ) ]
        |> List.filter (\( p1, p2 ) -> not (p2 == Nothing))
        |> List.map (\( p1, p2 ) -> ( p1, Maybe.withDefault "" p2 ))


initialModel : Model
initialModel =
    Model LoginData.emptyLoginData "" Autocomplete.empty 5


updateConfig : Maybe GameOn -> Autocomplete.UpdateConfig Msg User
updateConfig activeInput =
    Autocomplete.updateConfig
        { toId = (LoginData.idGetter activeInput)
        , onKeyDown =
            \code maybeId ->
                if code == 38 || code == 40 then
                    Maybe.map PreviewUser maybeId
                else if code == 13 then
                    Maybe.map SelectUser maybeId
                else
                    Just <| Reset
        , onTooLow = Just <| Wrap True
        , onTooHigh = Just <| Wrap False
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
    | SteamAlternateChange Bool
    | ResponseError Http.Error
    | NoOp
    | Wrap Bool
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        xxx =
            Debug.log "msg:" msg
    in
        case msg of
            Wrap toTop ->
                if toTop then
                    { model
                        | autoState = Autocomplete.resetToFirstItem (updateConfig model.data.activeUsername) (Array.toList model.data.possibleUsers) model.showHowMany model.autoState
                    }
                        ! []
                else
                    { model
                        | autoState = Autocomplete.resetToLastItem (updateConfig model.data.activeUsername) (Array.toList model.data.possibleUsers) model.showHowMany model.autoState
                    }
                        ! []

            SetAutoState autoMsg ->
                let
                    ( newState, maybeMsg ) =
                        Autocomplete.update (updateConfig model.data.activeUsername) autoMsg model.showHowMany model.autoState (Array.toList model.data.possibleUsers)

                    newModel =
                        { model | autoState = newState }
                in
                    case maybeMsg of
                        Nothing ->
                            newModel ! []

                        Just updateMsg ->
                            update updateMsg newModel

            SelectUser name ->
                { model | message = "", data = LoginData.selectUser name model.data } ! []

            PreviewUser name ->
                { model | message = "", data = LoginData.previewUser name model.data } ! []

            CreateUpdateUser u ->
                { model | message = "" }
                    ! [ serializeUser u |> routes.login.createUpdate |> (getResponse UserFetched) ]

            UserFetched u ->
                { model | message = "", data = LoginData.setUser u model.data } ! []

            UsersFetched users ->
                { model | message = "", data = LoginData.setPossibleUsers users model.data } ! []

            SteamChange u ->
                { model | message = "", data = LoginData.updateSteamUsername u model.data } ! []

            GogChange u ->
                { model | message = "", data = LoginData.updateGogUsername u model.data } ! []

            SteamGainFocus ->
                { model | message = "", data = LoginData.changeActiveUsername Steam model.data }
                    ! [ sendUsersRequest routes.login.fetchSteam model.data.user (Maybe.withDefault "" model.data.user.steamUsername) ]

            GogGainFocus ->
                { model | message = "", data = LoginData.changeActiveUsername Gog model.data }
                    ! [ sendUsersRequest routes.login.fetchGog model.data.user (Maybe.withDefault "" model.data.user.gogUsername) ]

            SteamAlternateChange c ->
                let
                    oldUser =
                        model.data.user

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

                    sendUpdate _ =
                        serializeUser model.data.user |> changeAlternate |> routes.login.steamAlternate |> (getResponse UserFetched)

                    newModel =
                        { model | message = "", data = LoginData.setUser newUser model.data }
                in
                    newModel ! [ Maybe.map sendUpdate newModel.data.user.id |> Maybe.withDefault Cmd.none ]

            ResponseError err ->
                { model | message = toString err } ! []

            NoOp ->
                model ! []

            Reset ->
                { model | autoState = Autocomplete.reset (updateConfig model.data.activeUsername) model.autoState } ! []


sendUsersRequest method user username =
    if String.length username == 0 then
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
                , id (LoginData.idGetter activeInput user)
                ]
            , children = [ Html.text (LoginData.idGetter activeInput user) ]
            }
    in
        Autocomplete.viewConfig
            { toId = (LoginData.idGetter activeInput)
            , ul = [ class "autocomplete-list" ]
            , li = customizedLi
            }


view : Model -> Html Msg
view model =
    div [ class "center" ] <| List.concat [ usernameForm model, createUpdateButton model, mainPageLink model ]


createUpdateButton model =
    if model.data.userLoaded then
        []
    else
        [ form [] [ button [ type_ "button", onClick <| CreateUpdateUser model.data.user ] [ text "Create/Update" ], br [] [] ] ]


usernameForm model =
    let
        userData maybeUser =
            ( maybeUser |> Maybe.andThen .steamUsername |> Maybe.withDefault "", maybeUser |> Maybe.andThen .gogUsername |> Maybe.withDefault "", maybeUser |> Maybe.map .steamAlternate |> Maybe.withDefault False )

        ( typedSteamUsername, typedGogUsername, typedSteamAlternate ) =
            userData (Just model.data.user)
    in
        [ form [ onSubmit NoOp ]
            [ span [] [ text model.message ]
            , steamInput model typedSteamUsername
            , alternateSteamInput model.data
            , gogInput model typedGogUsername
            , div [ class "form-group" ] [ input [ type_ "submit", style [ ( "display", "none" ) ] ] [] ]
            ]
        , br [] []
        ]


alternateSteamInput data =
    div [ class "form-group" ]
        [ label []
            [ text "Alternate Steam login:"
            , br [] []
            , input
                [ type_ "checkbox"
                , disabled (not data.userLoaded)
                , checked data.user.steamAlternate
                , onCheck SteamAlternateChange
                ]
                []
            ]
        ]


steamInput model typed =
    usernameInput model (model.data.activeUsername == Just Steam) "Steam username:" SteamChange SteamGainFocus typed


gogInput model typed =
    usernameInput model (model.data.activeUsername == Just Gog) "Gog username:" GogChange GogGainFocus typed


usernameInput model show name inputMsg focusMsg typed =
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
                , input
                    [ type_ "text"
                    , disabled model.data.userLoaded
                    , id <| inputId focusMsg
                    , onInput inputMsg
                    , onFocus focusMsg
                    , value typed
                    , onWithOptions "keydown" options dec
                    ]
                    []
                , if show then
                    Html.map SetAutoState (Autocomplete.view (viewConfig model.data.activeUsername) model.showHowMany model.autoState (Array.toList model.data.possibleUsers))
                  else
                    div [] []
                ]
            ]


inputId focusMsg =
    "input_ " ++ toString focusMsg


mainPageLink model =
    model.data.user.id
        |> Maybe.map
            (\userId ->
                [ form [ method "get", Router.mainPageUrl [] |> action ]
                    [ input [ type_ "hidden", name "sources", value <| toString WishList ] []
                    , input [ type_ "hidden", name "userId", value <| toString userId ] []
                    , button [ type_ "submit" ] [ text "Continue" ]
                    ]
                ]
            )
        |> Maybe.withDefault []
