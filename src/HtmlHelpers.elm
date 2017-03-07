module HtmlHelpers exposing (..)

import Html
import Html.Events exposing (on, targetValue)
import Json.Decode as Json


onSelect : (String -> a) -> Html.Attribute a
onSelect msg =
    on "change" (Json.map msg targetValue)
