module UI.Radio exposing (group)

{-|


# Grouped Elements

@docs group

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


group : RenderConfig -> String -> (a -> msg) -> Maybe a -> List ( a, String ) -> Element msg
group renderConfig label message selected buttons =
    buttons
        |> List.map
            (\( id, buttonLabel ) ->
                radioButton renderConfig
                    (message id)
                    buttonLabel
                    (selected == Just id)
            )
        |> Element.column
            (Element.spacing 8
                :: (ARIA.toElementAttributes <| ARIA.roleRadioGroup label)
            )


radioButton : RenderConfig -> msg -> String -> Bool -> Element msg
radioButton renderConfig message label state =
    let
        radioAttrs =
            [ Element.width (px 26)
            , Element.height (px 26)
            , Border.color Palette.primary.middle
            , Border.width 3
            , Border.rounded 26
            , Events.onClick message
            , Element.pointer
            ]
                ++ (ARIA.toElementAttributes <| ARIA.rolePresentation)

        radioIcon =
            if state then
                Element.el
                    (radioSelected :: radioAttrs)
                    (radioCheck renderConfig)

            else
                Element.el
                    radioAttrs
                    Element.none

        rowAttrs =
            Element.spacing 8
                :: (ARIA.toElementAttributes <| ARIA.roleRadio state)
    in
    Element.row rowAttrs
        [ radioIcon
        , Text.caption label
            |> Text.renderElement renderConfig
        ]


radioSelected : Attribute msg
radioSelected =
    Background.color Palette.primary.middle


radioCheck : RenderConfig -> Element msg
radioCheck renderConfig =
    Icon.check "Selected"
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
