module UI.Checkbox exposing
    ( Checkbox, checkbox
    , withLabelVisible
    , renderElement
    )

{-| Accessible and uniform-styled implementation of a checkbox.

    Checkbox.checkbox "I agree with terms of service."
        Msg.ToggleThis
        True
        |> Checkbox.withLabelVisible False
        |> Checkbox.renderElement renderConfig


# Building

@docs Checkbox, checkbox


# Label

@docs withLabelVisible


# Rendering

@docs renderElement

-}

import Element exposing (Attribute, Element, px)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html.Attributes as HtmlAttrs
import UI.Icon as Icon
import UI.Internal.Colors as Colors
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Internal.SelectionControl as SelectionControl
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.ARIA as ARIA
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


type alias CheckboxSize =
    SelectionControl.SelectionControlSize


{-| Defines all the required properties for creating a checkbox.

    checkbox "Buy Milk"
        (Msg.ProductListSet Milk)
        True

-}
checkbox : String -> (Bool -> msg) -> Bool -> Checkbox msg
checkbox label message state =
    Checkbox { message = message, label = label, state = state }
        { labelVisible = True, size = SelectionControl.SizeSM }


{-| Show or hide the checkbox's label.

    TextField.setLabelVisible True someTextField

-}
withLabelVisible : Bool -> Checkbox msg -> Checkbox msg
withLabelVisible bool (Checkbox prop opt) =
    Checkbox prop { opt | labelVisible = bool }


withSize : CheckboxSize -> Checkbox msg -> Checkbox msg
withSize size (Checkbox prop opt) =
    Checkbox prop { opt | size = size }


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement_ : RenderConfig -> Checkbox msg -> Element msg
renderElement_ renderConfig (Checkbox { message, label, state } { labelVisible }) =
    let
        boxAttrs =
            Element.width (px 20)
                :: Element.height (px 20)
                :: Border.color Colors.primary.middle
                :: Border.width 2
                :: Border.rounded 8
                :: Element.onIndividualClick (message (not state))
                :: Element.pointer
                :: aria

        aria =
            ARIA.roleCheckbox state
                |> ARIA.withLabel label
                |> ARIA.toElementAttributes

        boxIcon =
            if state then
                Element.el
                    (Background.color Colors.primary.middle :: boxAttrs)
                    (boxCheck renderConfig)

            else
                Element.el
                    boxAttrs
                    Element.none
    in
    if labelVisible then
        Element.row [ Element.spacing 8 ]
            [ boxIcon
            , Text.caption label
                |> Text.renderElement renderConfig
            ]

    else
        boxIcon


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Checkbox msg -> Element msg
renderElement renderConfig (Checkbox { message, label, state } { size }) =
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
    in
    Input.checkbox
        (SelectionControl.buttonAttributes size state)
        { onChange = message
        , icon = boxIcon
        , checked = state
        , label = Input.labelRight [ Element.width Element.fill ] (SelectionControl.label renderConfig label)
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
