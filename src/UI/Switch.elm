module UI.Switch exposing
    ( Switch, switch
    , withActivatedColor
    , ActivatedColor, success, danger
    , renderElement
    )

{-| Accessible and uniform-styled implementation of a switch.

    Switch.switch "Flying mode."
        Msg.ToggleThis
        True
        |> Switch.withActivatedColor Switch.success
        |> Switch.renderElement renderConfig


# Building

@docs Switch, switch


# Coloring

@docs withActivatedColor
@docs ActivatedColor, success, danger


# Rendering

@docs renderElement

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import UI.Internal.Colors as Colors
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
    { activatedColor : ActivatedColor
    }


{-| Colors can be used to enforce a different kind of attention to the switch when activated.

The default color is half-opaque black.

-}
type ActivatedColor
    = NoColor
    | ColorSuccess
    | ColorDanger


{-| Defines all the required properties for creating a switch.

    switch "Flight mode"
        (Msg.SetFlightMode <| not model.flightModeEnabled)
        model.flightModeEnabled

-}
switch : String -> (Bool -> msg) -> Bool -> Switch msg
switch label message state =
    Switch { message = message, label = label, state = state }
        { activatedColor = NoColor }


{-| Change the switch's color when activated.

    TextField.setLabelVisible True someTextField

-}
withActivatedColor : ActivatedColor -> Switch msg -> Switch msg
withActivatedColor activatedColor (Switch prop opt) =
    Switch prop { opt | activatedColor = activatedColor }


{-| This is the success-color, and it's greenish for enforcing success.

    Switch.withActivatedColor Switch.success
        someSwitch

-}
success : ActivatedColor
success =
    ColorSuccess


{-| This is the danger-color, and it's reddish for enforcing the user's attention.

    Switch.withActivatedColor Switch.danger
        someSwitch

-}
danger : ActivatedColor
danger =
    ColorDanger


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Switch msg -> Element msg
renderElement _ (Switch { message, label, state } { activatedColor }) =
    let
        boxAttrs =
            Border.rounded 32
                :: Element.onIndividualClick (message (not state))
                :: Element.pointer
                :: aria

        aria =
            ARIA.roleSwitch state
                |> ARIA.withLabel label
                |> ARIA.toElementAttributes

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

        thinBorder =
            Element.el
                [ Border.shadow
                    { offset = ( 0, 3 )
                    , size = 0
                    , blur = 1
                    , color = Element.rgba 0 0 0 0.06
                    }
                , Border.rounded 10
                ]

        activatedColorAttr =
            case activatedColor of
                NoColor ->
                    Colors.overlayBackground

                ColorSuccess ->
                    Background.color Colors.success.middle

                ColorDanger ->
                    Background.color Colors.danger.middle
    in
    if state then
        Element.none
            |> Element.el (Element.padding 10 :: knobAttrs)
            |> thinBorder
            |> Element.el
                (activatedColorAttr
                    :: boxAttrs
                    ++ Element.transition True paddingTransition
                )

    else
        Element.none
            |> Element.el
                (Border.color (Element.rgba 0 0 0 0.1)
                    :: Border.width 1
                    :: Element.padding 9
                    :: knobAttrs
                )
            |> thinBorder
            |> Element.el
                (Background.color Colors.gray.lighter
                    :: boxAttrs
                    ++ Element.transition False paddingTransition
                )


paddingTransition : Element.Transition msg
paddingTransition =
    Element.easyPadding
        { top = 1, bottom = 1, left = 1, right = 20 }
        { top = 1, bottom = 1, left = 20, right = 1 }
