module UI.Utils.Focus exposing
    ( Focus, focus
    , withOnEnter, withOnLeave, withTabIndex
    , toElementAttributes
    )

{-| Unified configuration for managing focus on components.


# Configuration

@docs Focus, focus
@docs withOnEnter, withOnLeave, withTabIndex


# Element Attributes

@docs toElementAttributes

-}

import Element exposing (Attribute)
import Element.Events as Events
import Element.Input as Input
import Html.Attributes as HtmlAttrs


{-| Required configuration for managing focus.
-}
type Focus msg
    = Focus { hasFocus : Bool } (Optional msg)


type alias Optional msg =
    { onEnter : Maybe msg
    , onLeave : Maybe msg
    , tabIndex : Maybe Int
    }


{-| Allow paack-ui to know the one useful thing for visually focus feedback: Is it focused?

    Focus.focus True

-}
focus : Bool -> Focus msg
focus hasFocus =
    Focus { hasFocus = hasFocus }
        { onEnter = Nothing
        , onLeave = Nothing
        , tabIndex = Nothing
        }


{-| Allow paack-ui to message a feedback when focus enters the element.

    Focus.focus model.isInputFocused
        |> Focus.withOnEnter Msg.InputFocused

-}
withOnEnter : msg -> Focus msg -> Focus msg
withOnEnter msg (Focus prop opt) =
    Focus prop { opt | onEnter = Just msg }


{-| Allow paack-ui to message a feedback when focus leaves the element.

    Focus.focus model.isInputFocused
        |> Focus.withOnLeave Msg.InputFocusLost

-}
withOnLeave : msg -> Focus msg -> Focus msg
withOnLeave msg (Focus prop opt) =
    Focus prop { opt | onLeave = Just msg }


{-| Makes the element keyboard-interactive.

    Focus.focus model.isInputFocused
        |> Focus.withTabIndex -1

-}
withTabIndex : Int -> Focus msg -> Focus msg
withTabIndex value (Focus prop opt) =
    Focus prop { opt | tabIndex = Just value }


{-| Applies [`Focus`]`#Focus` into Elm UI attributes.

    Element.el [ focusAttributes someConfig ] <|
        Element.text "Some content"

-}
toElementAttributes : Focus msg -> List (Attribute msg)
toElementAttributes (Focus { hasFocus } { onEnter, tabIndex, onLeave }) =
    let
        withFocusAttrs attributes =
            if hasFocus then
                Input.focusedOnLoad :: attributes

            else
                attributes

        withOnEnterAttrs =
            Maybe.map
                (Events.onFocus >> (::))
                onEnter
                |> Maybe.withDefault identity

        withOnLeaveAttrs =
            Maybe.map
                (Events.onLoseFocus >> (::))
                onLeave
                |> Maybe.withDefault identity

        tabAttrs =
            case tabIndex of
                Just tabIndexInt ->
                    tabIndexInt
                        |> HtmlAttrs.tabindex
                        |> Element.htmlAttribute
                        |> List.singleton

                Nothing ->
                    []
    in
    tabAttrs |> withOnEnterAttrs |> withOnLeaveAttrs |> withFocusAttrs
