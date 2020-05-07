module Icons exposing (stories)

import Element
import Element.Background as Background
import Element.Font as Font
import UI.Icon as Icon
import UI.Theme as Theme
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
    [ ( Icon.close, "Close" )
    , ( Icon.toggle, "Toggle" )
    ]


iconView cfg ( iconFn, label ) =
    Element.column
        [ Background.color Theme.primary
        , Font.color Theme.white
        , Element.spacing 10
        , Element.padding 10
        ]
        [ iconFn label
            |> Icon.toEl cfg
            |> Element.el
                [ Font.size 25
                , Element.centerX
                ]
        , Element.el [ Font.size 14 ] <| Element.text label
        ]


iconsView cfg =
    icons
        |> List.map (iconView cfg)
        |> Element.wrappedRow
            [ Element.spacing 10
            ]
        |> Element.layout []
