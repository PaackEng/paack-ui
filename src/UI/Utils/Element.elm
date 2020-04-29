module UI.Utils.Element exposing (disabled, letterSpacing)

import Element exposing (Attribute)
import Html.Attributes as HtmlAttrs


letterSpacing : Float -> Attribute msg
letterSpacing px =
    -- TODO: Allow other units
    (String.fromFloat px ++ "px")
        |> HtmlAttrs.style "letter-spacing"
        |> Element.htmlAttribute


disabled : List (Attribute msg)
disabled =
    [ ( "disabled", "true" )
    , ( "aria-disabled", "true" )
    , ( "tabindex", "-1" )
    , ( "pointer-events", "none" )
    , ( "cursor", "default" )
    ]
        |> List.map pairToAttribute



-- Helpers


pairToAttribute : ( String, String ) -> Attribute msg
pairToAttribute ( k, v ) =
    Element.htmlAttribute <| HtmlAttrs.style k v


boolToHTML : Bool -> String
boolToHTML val =
    if val then
        "true"

    else
        "false"
