module UI.Utils.Element exposing (letterSpacing)

import Element exposing (Attribute)
import Html.Attributes as HtmlAttrs


letterSpacing : Float -> Attribute msg
letterSpacing px =
    -- TODO: Allow other units
    (String.fromFloat px ++ "px")
        |> HtmlAttrs.style "letter-spacing"
        |> Element.htmlAttribute
