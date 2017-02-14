module GameOptionsDialog exposing (model, emptyModel, view, fetch, Model)

import Dialog
import Html exposing (Html, div, h2, h3, p, text)
import Http
import Model exposing (GameOptions)
import Router


-- MODEL


type alias Model =
    { gameOptions : Maybe GameOptions }


model options =
    Model <| Just options


emptyModel =
    Model Nothing



-- UPDATE


fetch : Maybe Int -> (GameOptions -> c) -> (Http.Error -> c) -> Cmd c
fetch steamId mess err =
    let
        send id =
            Http.send (Router.resolveResponse mess err) <| Router.fetchGameOptions [ ( "gameId", toString id ) ]
    in
        Maybe.map send steamId |> Maybe.withDefault Cmd.none



-- VIEW


view : msg -> Model -> Html msg
view mess model =
    Dialog.view <|
        Maybe.map
            (\o ->
                { closeMessage = Just mess
                , containerClass = Just "your-container-class"
                , header = Just (h2 [] [ text "Game Options" ])
                , body =
                    Just
                        (div []
                            [ h3 [] [ text "Name:" ]
                            , p [] [ text o.entry.name ]
                            , p [] [ text "Let me tell you something important..." ]
                            , p [] [ text "Let me tell you something important..." ]
                            ]
                        )
                , footer = Nothing
                }
            )
            model.gameOptions



--type alias GameQuery =
--    { query : String, site : String, results : List String }
--
--
--type alias GameOptions =
--    { entry : SteamEntry, queries : List GameQuery }
