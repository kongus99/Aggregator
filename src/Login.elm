module Login exposing (..)

import Array exposing (Array)
import Autocomplete
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Dom
import Html exposing (Html, a, br, button, div, form, input, label, span, text)
import Html.Attributes exposing (action, autocomplete, checked, class, classList, disabled, href, id, method, name, style, type_, value)
import Html.Events exposing (keyCode, onBlur, onCheck, onClick, onFocus, onInput, onSubmit, onWithOptions)
import Http
import Json.Decode as Json
import LoginData exposing (LoginData)
import Model exposing (..)
import Router exposing (routes)
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
        { toId = LoginData.idGetter activeInput
        , onKeyDown =
            \code maybeId ->
                if code == 38 || code == 40 then
                    Just NoOp
                else if code == 13 then
                    Maybe.map SelectUser maybeId
                else
                    Just Reset
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
    case msg of
        Wrap toTop ->
            if toTop then
                { model
                    | autoState = Autocomplete.resetToFirstItem (updateConfig model.data.activeUsername) (LoginData.filterUsers model.data) model.showHowMany model.autoState
                }
                    ! []
            else
                { model
                    | autoState = Autocomplete.resetToLastItem (updateConfig model.data.activeUsername) (LoginData.filterUsers model.data) model.showHowMany model.autoState
                }
                    ! []

        SetAutoState autoMsg ->
            let
                ( newState, maybeMsg ) =
                    Autocomplete.update (updateConfig model.data.activeUsername) autoMsg model.showHowMany model.autoState (LoginData.filterUsers model.data)

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

        CreateUpdateUser u ->
            { model | message = "" }
                ! [ serializeUser u |> routes.login.createUpdate |> .request |> getResponse UserFetched ]

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
                ! [ sendUsersRequest model.data.user (Maybe.withDefault "" model.data.user.steamUsername) ]

        GogGainFocus ->
            { model | message = "", data = LoginData.changeActiveUsername Gog model.data }
                ! [ sendUsersRequest model.data.user (Maybe.withDefault "" model.data.user.gogUsername) ]

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
                    serializeUser model.data.user |> changeAlternate |> routes.login.steamAlternate |> .request |> getResponse UserFetched

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


sendUsersRequest user username =
    if String.length username == 0 then
        serializeUser user |> routes.login.fetchUsers |> .request |> getResponse UsersFetched
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
        { toId = LoginData.idGetter activeInput
        , ul = [ class "autocomplete-list" ]
        , li = customizedLi
        }


view : Model -> Html Msg
view model =
    div [] [ usernameForm model, actionButton model ]


usernameForm model =
    let
        userData maybeUser =
            ( maybeUser |> Maybe.andThen .steamUsername |> Maybe.withDefault "", maybeUser |> Maybe.andThen .gogUsername |> Maybe.withDefault "", maybeUser |> Maybe.map .steamAlternate |> Maybe.withDefault False )

        ( typedSteamUsername, typedGogUsername, typedSteamAlternate ) =
            userData (Just model.data.user)
    in
    Form.form [ onSubmit NoOp ]
        [ Form.row [] [ Form.col [ Col.offsetXs3, Col.xs6 ] [ span [] [ text model.message ] ] ]
        , steamInput model typedSteamUsername
        , alternateSteamInput model.data
        , gogInput model typedGogUsername
        ]


alternateSteamInput data =
    let
        buttonCheck =
            if data.userLoaded then
                Button.attrs [ onCheck SteamAlternateChange ]
            else
                Button.disabled True

        buttonText =
            if data.user.steamAlternate then
                "Steam login method : Alternate"
            else
                "Steam login method : Normal"
    in
    Form.row []
        [ Form.col [ Col.offsetXs5, Col.xs2 ]
            [ ButtonGroup.checkboxButtonGroup [ ButtonGroup.attrs [ class "btn-block" ] ]
                [ ButtonGroup.checkboxButton data.user.steamAlternate
                    [ buttonCheck
                    , Button.secondary
                    , Button.block
                    ]
                    [ text buttonText ]
                ]
            ]
        ]


steamInput model typed =
    usernameInput model (Just Steam) "Steam username" SteamChange SteamGainFocus typed


gogInput model typed =
    usernameInput model (Just Gog) "Gog username" GogChange GogGainFocus typed


usernameInput model activeInput name inputMsg focusMsg typed =
    let
        filteredUsers =
            LoginData.filterUsers model.data
    in
    Form.row []
        [ Form.col [ Col.offsetXs5, Col.xs2 ]
            [ Input.text
                [ Input.disabled model.data.userLoaded
                , Input.placeholder name
                , Input.onInput inputMsg
                , Input.attrs [ onFocus focusMsg ]
                , Input.value typed
                ]
            , if model.data.activeUsername == activeInput && not (List.isEmpty filteredUsers) then
                Html.map SetAutoState (Autocomplete.view (viewConfig model.data.activeUsername) model.showHowMany model.autoState filteredUsers)
              else
                div [] []
            ]
        ]


actionButton model =
    model.data.user.id
        |> Maybe.map
            (\userId ->
                Form.row []
                    [ Form.col [ Col.offsetXs5, Col.xs2 ]
                        [ a [ href <| (routes.main.page [ ( "sources", toString WishList ), ( "userId", toString userId ) ]).url ]
                            [ Button.button [ Button.secondary ] [ text "Continue" ]
                            ]
                        ]
                    ]
            )
        |> Maybe.withDefault
            (Form.row []
                [ Form.col [ Col.offsetXs5, Col.xs2 ]
                    [ Button.button [ Button.secondary, Button.onClick <| CreateUpdateUser model.data.user ] [ text "Create/Update" ] ]
                ]
            )
