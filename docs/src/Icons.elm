module Icons exposing (stories)

import Element
import Element.Background as Background
import Element.Font as Font
import Html
import UI.Icon as Icon
import UI.Internal.Palette as Palette
import UIExplorer exposing (storiesOf)


stories cfg =
    storiesOf
        "Icons"
        [ ( "IconsExample"
          , \_ -> iconsView cfg
          , { note = "" }
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
    , ( Icon.leftArrow, "LeftArrow: Wrong name in the svg" )
    ]


iconView cfg ( iconFn, label ) =
    Element.column
        [ Background.color Palette.primary.middle
        , Font.color Palette.contrastPrimary.middle
        , Element.spacing 10
        , Element.padding 10
        ]
        [ iconFn label
            |> Icon.toEl cfg
            |> Element.el [ Element.centerX ]
        , Element.el [ Font.size 14 ] <| Element.text label
        ]


iconsView cfg =
    icons
        |> List.map (iconView cfg)
        |> (::) svgSprite
        |> Element.wrappedRow
            [ Element.spacing 10
            ]
        |> Element.layout []


svgSprite =
    Element.el
        []
        (Element.html
            (Html.node "svg-import"
                []
                []
            )
        )
