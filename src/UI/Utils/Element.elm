module UI.Utils.Element exposing (colorTransition, disabled)

import Element exposing (Attribute)
import Html.Attributes as HtmlAttrs


disabled : List (Attribute msg)
disabled =
    [ ( "disabled", "true" )
    , ( "aria-disabled", "true" )
    , ( "tabindex", "-1" )
    , ( "pointer-events", "none" )
    , ( "cursor", "default" )
    ]
        |> List.map stylePair


colorTransition : Int -> List (Attribute msg)
colorTransition time =
    [ ( "transition-property", "color, background-color, border-color" )
    , ( "transition-duration", String.fromInt time ++ "ms" )
    , ( "transition-timing-function", "linear" )
    ]
        |> List.map stylePair



-- Helpers


stylePair : ( String, String ) -> Attribute msg
stylePair ( k, v ) =
    Element.htmlAttribute <| HtmlAttrs.style k v
