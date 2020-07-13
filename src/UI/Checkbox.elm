module UI.Checkbox exposing (checkbox)

{-|


# Atomic Elements

@docs checkbox

**NOTE**:
We have plans for "checklists", but this won't be created until usecase appears.

-}

import Element exposing (Element)
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text


checkbox : RenderConfig -> (Bool -> msg) -> String -> Bool -> Element msg
checkbox renderConfig message label state =
    let
        boxAttrs =
            [ Element.width (px 26)
            , Element.height (px 26)
            , Border.color Palette.primary.middle
            , Border.width 3
            , Border.rounded 8
            , Events.onClick message
            , Element.pointer
            ]
                ++ (ARIA.toElementAttributes <| ARIA.roleButton)

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
