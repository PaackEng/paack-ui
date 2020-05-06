module UI.Utils.Element exposing (Focus, colorTransition, disabled)

import Element exposing (Attribute)
import Html.Attributes as HtmlAttrs


type alias Focus msg =
    { onEnter : msg
    , tabIndex : Int
    , hasFocus : Bool
    }


disabled : List (Attribute msg)
disabled =
    [ HtmlAttrs.attribute "disabled" "true"
    , HtmlAttrs.attribute "aria-disabled" "true"
    , HtmlAttrs.attribute "tabindex" "-1"
    , HtmlAttrs.attribute "pointer-events" "none"
    , HtmlAttrs.style "cursor" "default"
    ]
        |> List.map Element.htmlAttribute


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
