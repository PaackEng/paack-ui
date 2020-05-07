module UI.Utils.Focus exposing (Focus, focusAttributes)

import Element exposing (Attribute)
import Element.Events as Events
import Element.Input as Input
import Html.Attributes as HtmlAttrs


type alias Focus msg =
    { onEnter : msg
    , tabIndex : Int
    , hasFocus : Bool
    }


focusAttributes : Focus msg -> List (Attribute msg)
focusAttributes { onEnter, tabIndex, hasFocus } =
    let
        any =
            [ Events.onFocus onEnter
            , String.fromInt tabIndex
                |> HtmlAttrs.attribute "tabindex"
                |> Element.htmlAttribute
            ]
    in
    if hasFocus then
        Input.focusedOnLoad :: any

    else
        any
