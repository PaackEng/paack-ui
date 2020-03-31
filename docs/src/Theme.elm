module Theme exposing (stories)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import UI.Theme as Theme
import UIExplorer exposing (storiesOf)


stories =
    storiesOf
        "Colors"
        [ ( "Colors"
          , \_ -> colorsView
          , { note = "" }
          )
        ]


colors =
    [ ( Theme.primary, "Primary" )
    , ( Theme.success, "Success" )
    , ( Theme.error, "Error" )
    , ( Theme.warning, "Warning" )
    , ( Theme.black, "Black" )
    , ( Theme.white, "White" )
    , ( Theme.gray1, "Gray 1" )
    , ( Theme.gray2, "Gray 2" )
    , ( Theme.gray3, "Gray 3" )
    , ( Theme.gray4, "Gray 4" )
    ]


colorView ( colorFn, label ) =
    Element.el
        [ Background.color colorFn
        , Font.color
            (if colorFn == Theme.black then
                Theme.white

             else
                Theme.black
            )
        , Border.color Theme.black
        , Border.width 1
        , Element.spacing 10
        , Element.padding 10
        , Font.size 14
        ]
    <|
        Element.text label


colorsView =
    colors
        |> List.map colorView
        |> Element.wrappedRow
            [ Element.spacing 10
            ]
        |> Element.layout []
