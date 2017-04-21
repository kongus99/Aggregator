module HtmlHelpers exposing (onSelect, onMultiSelect, onEnter, onLinkClick, onMenuItemCheck, onMenuItemClick)

import Array
import Dict
import Html exposing (Attribute)
import Html.Events exposing (defaultOptions, keyCode, on, onWithOptions, targetChecked, targetValue)
import Json.Decode as Json


onLinkClick : msg -> Attribute msg
onLinkClick msg =
    onWithOptions "click" { defaultOptions | preventDefault = True } (Json.succeed msg)


onMenuItemCheck : (Bool -> msg) -> Attribute msg
onMenuItemCheck tagger =
    onWithOptions "click" { defaultOptions | stopPropagation = True } (Json.map tagger targetChecked)


onMenuItemClick : msg -> Attribute msg
onMenuItemClick msg =
    onWithOptions "click" { defaultOptions | stopPropagation = True } (Json.succeed msg)


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
