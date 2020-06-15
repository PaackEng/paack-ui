module Icons exposing (stories)

import Element
import Element.Background as Background
import Element.Font as Font
import Html
import UI.Icon as Icon
import UI.Palette as Palette
import UIExplorer exposing (storiesOf)


stories cfg =
    storiesOf
        "Icons"
        [ ( "IconsExample"
          , \_ -> iconsView cfg
          , { note =
                """
```elm
Icon.seeMore label
    |> Icon.withColor (Palette.color Palette.tonePrimary Palette.brightnessMiddle)
    |> Icon.toEl cfg
```
"""
            }
          )
        ]


icons =
    [ ( Icon.paackSpaces, "Spaces" )
    , ( Icon.search, "Search" )
    , ( Icon.packages, "Packages" )
    , ( Icon.warning, "Warning" )
    , ( Icon.print, "Print" )
    , ( Icon.add, "Add" )
    , ( Icon.logout, "Logout" )
    , ( Icon.warning, "Warning" )
    , ( Icon.seeMore, "See More" )
    , ( Icon.backwardContent, "Back" )
    , ( Icon.sandwichMenu, "Sandwich Menu" )
    , ( Icon.notifications, "Notifications" )
    , ( Icon.edit, "Edit" )
    ]


iconView cfg color ( iconFn, label ) =
    Element.column
        [ Element.spacing 10
        , Element.padding 10
        ]
        [ iconFn label
            |> Icon.withColor color
            |> Icon.toEl cfg
            |> Element.el [ Element.centerX ]
        , Element.el [ Font.size 14 ] <| Element.text label
        ]


iconsGroup cfg color =
    icons
        |> List.map (iconView cfg color)
        |> (::) svgSprite
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


svgSprite =
    Element.html
        (Html.node "paack-svg-icon-sprite"
            []
            []
        )
