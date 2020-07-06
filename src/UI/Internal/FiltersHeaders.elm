module UI.Internal.FiltersHeaders exposing (applied, normal)

import Element exposing (Attribute, Element, fill)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import UI.Button as Button
import UI.Icon as Icon
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Internal.Size as Size exposing (Size)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Utils.ARIA as ARIA



-- Mostly ripped from Button with Size.Small and WidthFull


normal : RenderConfig -> msg -> String -> Element msg
normal renderConfig openMsg label =
    -- Button.light
    Element.row (Events.onClick openMsg :: attrs False)
        [ Element.text label
        , Icon.add label
            |> Icon.withSize size
            |> Icon.renderElement renderConfig
            |> Element.el [ Element.alignRight ]
        ]


applied : RenderConfig -> msg -> msg -> String -> String -> Element msg
applied renderConfig openMsg clearMsg clearHint label =
    -- Button.primary
    Element.row (Events.onClick openMsg :: attrs True)
        [ Element.text label
        , Button.fromIcon (Icon.close clearHint)
            |> Button.cmd clearMsg Button.danger
            |> Button.withSize Size.ExtraSmall
            |> Button.renderElement renderConfig
            |> Element.el [ Element.alignRight ]
        ]


attrs : Bool -> List (Attribute msg)
attrs isApplied =
    let
        baseHeight =
            if isApplied then
                24

            else
                16

        paddingXY =
            Element.paddingXY
                ((36 - 16) // 2 - border)
                ((36 - baseHeight) // 2 - border)

        workingTheme =
            if isApplied then
                [ Background.color Palette.primary.middle
                , Border.color Palette.primary.middle
                , Palette.color
                    Palette.tonePrimary
                    Palette.brightnessMiddle
                    |> Palette.setContrasting True
                    |> Palette.toElementColor
                    |> Font.color
                ]

            else
                [ Background.color Palette.gray.lightest
                , Border.color Palette.gray.lightest
                , Font.color Palette.primary.darkest
                ]
    in
    [ Primitives.roundedBorders size
    , Element.width Element.fill
    , paddingXY
    , Element.spacing 8
    , Border.width border
    , Font.size textSize
    , Font.semiBold
    , Element.pointer
    ]
        ++ (ARIA.toElementAttributes <| ARIA.roleButton)
        ++ workingTheme


textSize : Int
textSize =
    12


border : Int
border =
    2


size : Size
size =
    Size.Small
