module Login exposing (..)

import Html exposing (Html, button, div, text, form, br, input, span, label)
import Html.Attributes exposing (action, type_, name, style, method, value, checked, disabled, class)
import Html.Events exposing (onBlur, onCheck, onClick, onFocus, onInput, onSubmit)
import Model exposing (..)
import Router exposing (routes, mainPageUrl)
import Http


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { loadedUser : Maybe User, enteredUser : User, possibleUsers : List User, message : String }


serializeUser : User -> List ( String, String )
serializeUser u =
    [ ( "steamUsername", u.steamUsername ), ( "steamAlternate", toString u.steamAlternate |> String.toLower |> Just ), ( "gogUsername", u.gogUsername ), ( "userId", Maybe.map toString u.id ) ]
        |> List.filter (\( p1, p2 ) -> not (p2 == Nothing))
        |> List.map (\( p1, p2 ) -> ( p1, Maybe.withDefault "" p2 ))


emptyUser =
    User Nothing (Just "") False (Just "")


initialModel : Model
initialModel =
    Model Nothing emptyUser [] ""



-- UPDATE


type Msg
    = SetUser
    | CreateUpdateUser
    | UserFetched User
    | UsersFetched (List User)
    | SteamChange SteamUsername
    | GogChange GogUserName
    | LoseFocus
    | SteamGainFocus
    | GogGainFocus
    | SteamAlternateChange Bool
    | ResponseError Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUser ->
            let
                loaded =
                    List.head model.possibleUsers

                entered =
                    Maybe.withDefault model.enteredUser loaded
            in
                ( { model | loadedUser = loaded, enteredUser = entered, message = "" }, Cmd.none )

        CreateUpdateUser ->
            ( { model | message = "" }, serializeUser model.enteredUser |> routes.login.createUpdate |> (getResponse UserFetched) )

        UserFetched u ->
            ( { model | loadedUser = Just u, enteredUser = u, message = "" }, Cmd.none )

        UsersFetched u ->
            ( { model | possibleUsers = u, message = "" }, Cmd.none )

        SteamChange u ->
            updateEnteredUser model routes.login.fetchSteam (\u -> \username -> { u | steamUsername = Just username }) u

        GogChange u ->
            updateEnteredUser model routes.login.fetchGog (\u -> \username -> { u | gogUsername = Just username }) u

        LoseFocus ->
            ( { model | message = "", possibleUsers = [] }, Cmd.none )

        SteamGainFocus ->
            ( { model | message = "" }, sendUsersRequest routes.login.fetchSteam model.enteredUser (Maybe.withDefault "" model.enteredUser.steamUsername) )

        GogGainFocus ->
            ( { model | message = "" }, sendUsersRequest routes.login.fetchGog model.enteredUser (Maybe.withDefault "" model.enteredUser.gogUsername) )

        SteamAlternateChange c ->
            let
                oldUser =
                    model.enteredUser

                newUser =
                    { oldUser | steamAlternate = c }

                changeAlternate args =
                    List.map
                        (\( p1, p2 ) ->
                            if p1 == "steamAlternate" then
                                ( p1, toString c |> String.toLower )
                            else
                                ( p1, p2 )
                        )
                        args

                sendUpdate u =
                    serializeUser u |> changeAlternate |> routes.login.steamAlternate |> (getResponse UserFetched)
            in
                ( { model | enteredUser = newUser, message = "" }, Maybe.map sendUpdate model.loadedUser |> Maybe.withDefault Cmd.none )

        ResponseError err ->
            ( { model | loadedUser = Nothing, message = toString err }, Cmd.none )


updateEnteredUser model method update username =
    let
        newUser =
            update model.enteredUser username
    in
        ( { model | enteredUser = newUser, message = "" }, sendUsersRequest method newUser username )


sendUsersRequest method user username =
    if String.length username > 1 then
        serializeUser user |> method |> (getResponse UsersFetched)
    else
        Cmd.none


getResponse : (a -> Msg) -> Http.Request a -> Cmd Msg
getResponse msg request =
    Http.send (Router.resolveResponse msg ResponseError) request



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "center" ] <| List.concat [ usernameForm model, createUpdateButton model, mainPageLink model ]


createUpdateButton model =
    Maybe.withDefault [ form [] [ button [ type_ "button", onClick CreateUpdateUser ] [ text "Create/Update" ], br [] [] ] ] <| Maybe.map (\_ -> []) model.loadedUser


usernameForm model =
    let
        userData maybeUser =
            ( Maybe.andThen .steamUsername maybeUser |> Maybe.withDefault "", Maybe.andThen .gogUsername maybeUser |> Maybe.withDefault "", Maybe.map .steamAlternate maybeUser |> Maybe.withDefault False )

        ( loadedSteamUsername, loadedGogUsername, loadedSteamAlternate ) =
            userData model.loadedUser

        ( enteredSteamUsername, enteredGogUsername, enteredSteamAlternate ) =
            userData (Just model.enteredUser)
    in
        [ form [ onSubmit SetUser ]
            [ span [] [ text model.message ]
            , steamInput loadedSteamUsername enteredSteamUsername
            , alternateSteamInput (model.loadedUser == Nothing) loadedSteamAlternate enteredSteamAlternate
            , gogInput loadedGogUsername enteredGogUsername
            , div [ class "form-group" ] [ input [ type_ "submit", style [ ( "display", "none" ) ] ] [] ]
            ]
        , br [] []
        ]


alternateSteamInput dis loaded entered =
    div [ class "form-group" ]
        [ label []
            [ text "Alternate Steam login:"
            , br [] []
            , text <| toString loaded
            , br [] []
            , input
                [ type_ "checkbox"
                , disabled dis
                , checked entered
                , onCheck SteamAlternateChange
                ]
                []
            ]
        ]


steamInput loaded entered =
    usernameInput "Steam username:" SteamChange SteamGainFocus loaded entered


gogInput loaded entered =
    usernameInput "Gog username:" GogChange GogGainFocus loaded entered


usernameInput name inputMsg focusMsg loaded entered =
    div [ class "form-group" ]
        [ label []
            [ text name
            , br [] []
            , text loaded
            , br [] []
            , input
                [ type_ "text"
                , onInput inputMsg
                , onFocus focusMsg
                , onBlur LoseFocus
                , value entered
                ]
                []
            ]
        ]


mainPageLink model =
    let
        userId =
            Maybe.withDefault 1 <| Maybe.andThen (\u -> u.id) model.loadedUser
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
                model.loadedUser
            )
