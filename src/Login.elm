module Login exposing (..)

import Html exposing (Html, button, div, text, form, br, input, span, label)
import Html.Attributes exposing (action, type_, name, style, method, value, checked, disabled, class)
import Html.Events exposing (onClick, onSubmit, onInput, onCheck)
import Model exposing (..)
import Router exposing (fetchUser, createUpdateUser, mainPageUrl, updateSteamAlternate)
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
    { loadedUser : Maybe User, enteredUser : User, message : String }


serializeUser : User -> List ( String, String )
serializeUser u =
    [ ( "steamUsername", u.steamUsername ), ( "steamAlternate", toString u.steamAlternate |> String.toLower |> Just ), ( "gogUsername", u.gogUsername ), ( "userId", Maybe.map toString u.id ) ]
        |> List.filter (\( p1, p2 ) -> not (p2 == Nothing))
        |> List.map (\( p1, p2 ) -> ( p1, Maybe.withDefault "" p2 ))


initialModel : Model
initialModel =
    Model Nothing (User Nothing (Just "") False (Just "")) ""



-- UPDATE


type Msg
    = FetchUser
    | CreateUpdateUser
    | UserFetched User
    | SteamUsernameChange SteamUsername
    | GogUsernameChange GogUserName
    | SteamAlternateChange Bool
    | ResponseError Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchUser ->
            ( { model | message = "" }, serializeUser model.enteredUser |> fetchUser |> getResponse )

        CreateUpdateUser ->
            ( { model | message = "" }, serializeUser model.enteredUser |> createUpdateUser |> getResponse )

        UserFetched u ->
            ( { model | loadedUser = Just u, enteredUser = u, message = "" }, Cmd.none )

        SteamUsernameChange u ->
            let
                oldUser =
                    model.enteredUser

                newUser =
                    { oldUser | steamUsername = Just u }
            in
                ( { model | enteredUser = newUser, message = "" }, Cmd.none )

        GogUsernameChange u ->
            let
                oldUser =
                    model.enteredUser

                newUser =
                    { oldUser | gogUsername = Just u }
            in
                ( { model | enteredUser = newUser, message = "" }, Cmd.none )

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
            in
                ( { model | enteredUser = newUser, message = "" }, Maybe.map (\u -> serializeUser u |> changeAlternate |> updateSteamAlternate |> getResponse) model.loadedUser |> Maybe.withDefault Cmd.none )

        ResponseError err ->
            ( { model | loadedUser = Nothing, message = toString err }, Cmd.none )


getResponse : Http.Request User -> Cmd Msg
getResponse request =
    Http.send (Router.resolveResponse UserFetched ResponseError) request



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
        [ form [ onSubmit FetchUser ]
            [ span [] [ text model.message ]
            , div [ class "form-group" ] [ label [] [ text "Steam username:", br [] [], text loadedSteamUsername, br [] [], input [ type_ "text", name "username1", onInput SteamUsernameChange, value enteredSteamUsername ] [] ] ]
            , div [ class "form-group" ] [ label [] [ text "Alternate Steam login:", br [] [], text <| toString loadedSteamAlternate, br [] [], input [ type_ "checkbox", name "Alternate", disabled <| model.loadedUser == Nothing, checked enteredSteamAlternate, onCheck SteamAlternateChange ] [] ] ]
            , div [ class "form-group" ] [ label [] [ text "Gog username:", br [] [], text loadedGogUsername, br [] [], input [ type_ "text", name "username2", onInput GogUsernameChange, value enteredGogUsername ] [] ] ]
            , div [ class "form-group" ] [ input [ type_ "submit", style [ ( "display", "none" ) ] ] [] ]
            ]
        , br [] []
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
