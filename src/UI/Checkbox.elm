module UI.Checkbox exposing
    ( Checkbox, checkbox
    , withHiddenLabel
    , CheckboxSize, withSize, sizeSM, sizeMD
    , renderElement
    )

{-| Accessible and uniform-styled implementation of a checkbox.

    Checkbox.checkbox "I agree with terms of service."
        Msg.ToggleThis
        True
        |> Checkbox.renderElement renderConfig


# Building

@docs Checkbox, checkbox


# Label

@docs withHiddenLabel


# Size

@docs CheckboxSize, withSize, sizeSM, sizeMD


# Rendering

@docs renderElement

-}

import Element exposing (Element, px)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import UI.Icon as Icon
import UI.Internal.Colors as Colors
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Internal.SelectionControl as SelectionControl exposing (SelectionControlSize(..))
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.Element as Element


{-| The `Checkbox msg` type is used for describing the component for later rendering.
-}
type Checkbox msg
    = Checkbox (Properties msg) Options


type alias Properties msg =
    { message : Bool -> msg
    , label : String
    , state : Bool
    }


type alias Options =
    { labelVisible : Bool
    , size : CheckboxSize
    }


{-| The different sizes the Checkbox can take
-}
type alias CheckboxSize =
    SelectionControl.SelectionControlSize


{-| Small-sized Checkbox
-}
sizeSM : CheckboxSize
sizeSM =
    SelectionControl.SizeSM


{-| Medium-sized Checkbox
-}
sizeMD : CheckboxSize
sizeMD =
    SelectionControl.SizeMD


{-| Defines all the required properties for creating a checkbox.

    checkbox "Buy Milk"
        (Msg.ProductListSet Milk)
        True

-}
checkbox : String -> (Bool -> msg) -> Bool -> Checkbox msg
checkbox label message state =
    Checkbox { message = message, label = label, state = state }
        { labelVisible = True, size = SelectionControl.SizeSM }


{-| Hide the checkbox's label.

    Checkbox.withHiddenLabel someCheckbox

-}
withHiddenLabel : Checkbox msg -> Checkbox msg
withHiddenLabel (Checkbox prop opt) =
    Checkbox prop { opt | labelVisible = False }


{-| `Checkbox.withSize` changes the size of the Checkbox

    Checkbox.withSize Checkbox.sizeMD someCheckbox

-}
withSize : CheckboxSize -> Checkbox msg -> Checkbox msg
withSize size (Checkbox prop opt) =
    Checkbox prop { opt | size = size }


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Checkbox msg -> Element msg
renderElement renderConfig (Checkbox { message, label, state } { labelVisible, size }) =
    let
        { icon, border } =
            SelectionControl.sizes size

        boxAttrs =
            [ Element.width (px icon)
            , Element.height (px icon)
            , Border.color <| SelectionControl.iconColor state
            , Border.width border
            , Border.rounded 6
            , Element.onIndividualClick (message (not state))
            , Element.pointer
            ]

        boxIcon _ =
            if state then
                Element.el
                    (Background.color Colors.primary.middle :: boxAttrs)
                    (boxCheck renderConfig)

            else
                Element.el
                    boxAttrs
                    Element.none

        text =
            case size of
                SizeSM ->
                    Text.body2

                SizeMD ->
                    Text.body1

        labelElement =
            if labelVisible then
                Input.labelRight
                    [ Element.width Element.fill
                    , Element.paddingEach
                        { top = 3
                        , bottom = 0
                        , right = 0
                        , left = 0
                        }
                    ]
                    (text label |> Text.renderElement renderConfig)

            else
                Input.labelHidden label
    in
    Input.checkbox
        (SelectionControl.buttonAttributes size)
        { onChange = message
        , icon = boxIcon
        , checked = state
        , label = labelElement
        }


boxCheck : RenderConfig -> Element msg
boxCheck renderConfig =
    (renderConfig |> localeTerms >> .checkbox >> .toggle)
        |> Icon.check
        |> Icon.withCustomSize 14
        |> Icon.withColor
            (Palette.color
                Palette.tonePrimary
                Palette.brightnessMiddle
                |> Palette.setContrasting True
            )
        |> Icon.renderElement renderConfig
        |> Element.el
            [ Element.centerY
            , Element.centerX
            ]
