module GameOptionsDialog exposing (model, emptyModel, view, Model)

import Dialog
import Html exposing (Html, div, h2, h3, p, text)
import Model exposing (GameOptions)


-- MODEL


type alias Model =
    { gameOptions : Maybe GameOptions }


model options =
    Model <| Just options


emptyModel =
    Model Nothing



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
