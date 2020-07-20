module Icons exposing (stories)

import Element
import Element.Background as Background
import Element.Font as Font
import Html
import UI.Icon as Icon
import UI.Palette as Palette
import UIExplorer exposing (storiesOf)
import Utils exposing (iconsSvgSprite, prettifyElmCode)


stories cfg =
    storiesOf
        "Icons"
        [ ( "IconsExample"
          , \_ -> iconsView cfg
          , { code =
                prettifyElmCode
                    """
Icon.seeMore label
    |> Icon.withColor (Palette.color Palette.tonePrimary Palette.brightnessMiddle)
    |> Icon.renderElement cfg
"""
            , note = ""
            }
          )
        ]


icons =
    [ ( Icon.add, "Icon.Add" )
    , ( Icon.close, "Icon.close" )
    , ( Icon.edit, "Icon.edit" )
    , ( Icon.eventLog, "Icon.eventLog" )
    , ( Icon.logout, "Icon.logout" )
    , ( Icon.notifications, "Icon.notifications" )
    , ( Icon.paackSpaces, "Icon.paackSpaces" )
    , ( Icon.packages, "Icon.packages" )
    , ( Icon.print, "Icon.print" )
    , ( Icon.sandwichMenu, "Icon.sandwichMenu," )
    , ( Icon.search, "Icon.search" )
    , ( Icon.toggle, "Icon.toggle" )
    , ( Icon.toggleUp, "Icon.toggleUp" )
    , ( Icon.toggleDown, "Icon.toggleDown" )
    , ( Icon.previousContent, "Icon.previousContent" )
    , ( Icon.nextContent, "Icon.nextContent" )
    , ( Icon.seeMore, "Icon.seeMore" )
    , ( Icon.warning, "Icon.warning" )
    , ( Icon.filter, "Icon.filter" )
    ]


iconView cfg color ( iconFn, label ) =
    Element.column
        [ Element.spacing 10
        , Element.padding 10
        ]
        [ iconFn label
            |> Icon.withColor color
            |> Icon.renderElement cfg
            |> Element.el [ Element.centerX ]
        , Element.el [ Font.size 14 ] <| Element.text label
        ]


iconsGroup cfg color =
    icons
        |> List.map (iconView cfg color)
        |> (::) iconsSvgSprite
        |> Element.wrappedRow
            [ Element.spacing 10
            ]


iconsView cfg =
    Element.layout [] <|
        Element.column
            []
            [ iconsGroup cfg
                (Palette.color Palette.tonePrimary Palette.brightnessMiddle)
            , iconsGroup cfg
                (Palette.color Palette.toneWarning Palette.brightnessMiddle)
            , iconsGroup cfg
                (Palette.color Palette.toneGray Palette.brightnessMiddle)
            ]
