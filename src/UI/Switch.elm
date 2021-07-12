module UI.Switch exposing
    ( Switch, default, success, danger
    , renderElement
    )

{-| Accessible and uniform-styled implementation of a switch.

    Switch.default "Flying mode."
        Msg.ToggleThis
        True
        |> Switch.withColor Switch.success
        |> Switch.renderElement renderConfig


# Building

@docs Switch, default, success, danger


# Rendering

@docs renderElement

-}

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import UI.Internal.Colors as Colors
import UI.Internal.Utils.Element as Element
import UI.RenderConfig exposing (RenderConfig)
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element


{-| The `Switch msg` type is used for describing the component for later rendering.
-}
type Switch msg
    = Switch (Properties msg) Options


type alias Properties msg =
    { message : Bool -> msg
    , label : String
    , state : Bool
    }


type alias Options =
    { color : Color
    }


{-| Colors can be used to enforce a different kind of attention to the switch when activated.

The default color is half-opaque black.

-}
type Color
    = ColorHalfOpaque
    | ColorSuccess
    | ColorDanger


{-| Defines all the required properties for creating a switch. This default switch is half-opaque black.

    Switch.default "Flight mode"
        (Msg.SetFlightMode <| not model.flightModeEnabled)
        model.flightModeEnabled

-}
default : String -> (Bool -> msg) -> Bool -> Switch msg
default label message state =
    Switch { message = message, label = label, state = state }
        { color = ColorHalfOpaque }


{-| This switch has the success-color, and it's greenish for enforcing success.

    Switch.success "Toggle Firewall"
        (Msg.SetFirewall <| not model.hasFirewall)
        model.hasFirewall

-}
success : String -> (Bool -> msg) -> Bool -> Switch msg
success label message state =
    Switch { message = message, label = label, state = state }
        { color = ColorSuccess }


{-| This switch has the danger-color, and it's reddish for enforcing the user's attention.

    Switch.danger "Toggle root access"
        (Msg.SetRootAccess <| not model.hasRoot)
        model.hasRoot

-}
danger : String -> (Bool -> msg) -> Bool -> Switch msg
danger label message state =
    Switch { message = message, label = label, state = state }
        { color = ColorDanger }


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Switch msg -> Element msg
renderElement _ (Switch { message, label, state } { color }) =
    let
        boxAttrs =
            Border.rounded 32
                :: Element.onIndividualClick (message (not state))
                :: Element.pointer
                :: Element.paddingEach { top = 1, left = 1, right = 21, bottom = 1 }
                :: Element.style "transition" "background-color .2s"
                :: aria

        aria =
            ARIA.roleSwitch state
                |> ARIA.withLabel label
                |> ARIA.toElementAttributes

        colorAttr =
            case color of
                ColorHalfOpaque ->
                    Colors.overlayBackground

                ColorSuccess ->
                    Background.color Colors.green500

                ColorDanger ->
                    Background.color Colors.red600
    in
    if state then
        Element.none
            |> Element.el (Element.padding 10 :: knobAttrs)
            |> Element.el (paddingRight :: thinBorder)
            |> Element.el (colorAttr :: boxAttrs)

    else
        Element.none
            |> Element.el
                (Border.color (Element.rgba 0 0 0 0.1)
                    :: Border.width 1
                    :: Element.padding 9
                    :: knobAttrs
                )
            |> Element.el thinBorder
            |> Element.el (Background.color Colors.gray300 :: boxAttrs)


knobAttrs : List (Attribute msg)
knobAttrs =
    [ Border.rounded 10
    , Colors.mainBackground
    , Border.shadow
        { offset = ( 0, 3 )
        , size = 0
        , blur = 8
        , color = Element.rgba 0 0 0 0.15
        }
    ]


paddingRight : Attribute msg
paddingRight =
    Element.style "transform" "translateX(20px)"


thinBorder : List (Attribute msg)
thinBorder =
    [ Border.shadow
        { offset = ( 0, 3 )
        , size = 0
        , blur = 1
        , color = Element.rgba 0 0 0 0.06
        }
    , Border.rounded 10
    , Element.style "transition" "transform .4s"
    ]
