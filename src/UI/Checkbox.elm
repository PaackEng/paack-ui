module UI.Checkbox exposing (checkbox)

{-| Accessible and uniform-styled implementation of a checkbox.

@docs checkbox

-}

import Element exposing (Attribute, Element, px)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import UI.Icon as Icon
import UI.Internal.Palette as Palette
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element


{-| Atomic function that defines the component specifications and render to an Element.

    Checkbox.checkbox renderConfig
        Msg.SetRemember
        "Never ask again"
        model.rememberSelected

-}
checkbox : RenderConfig -> (Bool -> msg) -> String -> Bool -> Element msg
checkbox renderConfig message label state =
    let
        boxAttrs =
            [ Element.width (px 26)
            , Element.height (px 26)
            , Border.color Palette.primary.middle
            , Border.width 3
            , Border.rounded 8
            , Events.onClick (message (not state))
            , Element.pointer
            ]
                ++ (ARIA.toElementAttributes <| ARIA.roleCheckbox state)

        boxIcon =
            if state then
                Element.el
                    (boxSelected :: boxAttrs)
                    (boxCheck renderConfig)

            else
                Element.el
                    boxAttrs
                    Element.none
    in
    Element.row [ Element.spacing 8 ]
        [ boxIcon
        , Text.caption label
            |> Text.renderElement renderConfig
        ]


boxSelected : Attribute msg
boxSelected =
    Background.color Palette.primary.middle


boxCheck : RenderConfig -> Element msg
boxCheck renderConfig =
    Icon.check "Toggle"
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
