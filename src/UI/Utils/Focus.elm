module UI.Utils.Focus exposing
    ( Focus
    , toElementAttributes
    )

{-| Unified configuration for managing focus on components.


# Configuration

@docs Focus


# Element Attributes

@docs toElementAttributes

-}

import Element exposing (Attribute)
import Element.Events as Events
import Element.Input as Input
import Html.Attributes as HtmlAttrs


{-| Required configuration for managing focus.

    { onEnter = Msg.OnFocusEnterThisComponent
    , tabIndex = 2
    , hasFocus = False
    }

`hasFocus` can be used for enforcing focus when loading a new page.

-}
type alias Focus msg =
    { onEnter : msg
    , onLeave : msg
    , tabIndex : Int
    , hasFocus : Bool
    }


{-| Applies [`Focus`]`#Focus` into Elm UI attributes.

    Element.el [ focusAttributes someConfig ] <|
        Element.text "Some content"

-}
toElementAttributes : Focus msg -> List (Attribute msg)
toElementAttributes { onEnter, tabIndex, hasFocus, onLeave } =
    let
        any =
            [ Events.onFocus onEnter
            , Events.onLoseFocus onLeave
            , tabIndex
                |> HtmlAttrs.tabindex
                |> Element.htmlAttribute
            ]
    in
    if hasFocus then
        Input.focusedOnLoad :: any

    else
        any
