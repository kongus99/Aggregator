module HtmlHelpers exposing (onSelect, onMultiSelect, onEnter)

import Array
import Dict
import Html
import Html.Events exposing (keyCode, on, targetValue)
import Json.Decode as Json


onSelect : (String -> a) -> Html.Attribute a
onSelect msg =
    on "change" (Json.map msg targetValue)


onMultiSelect : (List String -> a) -> Html.Attribute a
onMultiSelect msg =
    on "change" (Json.map msg targetOptions)


onEnter : msg -> Html.Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)


type alias Option =
    { value : String }


targetOptions : Json.Decoder (List String)
targetOptions =
    Json.at [ "target", "selectedOptions" ] (Json.keyValuePairs (Json.maybe (Json.field "value" Json.string)))
        |> Json.map (List.filterMap Tuple.second)
