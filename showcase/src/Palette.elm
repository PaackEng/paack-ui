module Palette exposing (stories)

import Element exposing (Element, px)
import Element.Background as Background
import Element.Font as Font
import PluginOptions exposing (defaultWithoutMenu)
import UI.Palette.V2 as Palette exposing (Hue, Shade)
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerUI, goToDocsCallToAction, prettifyElmCode)


stories : ExplorerUI
stories =
    storiesOf
        "Colors"
        [ ( "Colors"
          , \_ -> Element.layout [] colorsView
          , { defaultWithoutMenu
                | code =
                    prettifyElmCode code
                , note =
                    note
            }
          )
        ]


code : String
code =
    """
import UI.Palette as Palette

myRedSquare =
    Element.el
        [ Palette.toBackgroundColor Palette.red
        , Element.width (px 100)
        , Element.height (px 100)
        , Element.spacing 10
        , Element.padding 10
        , Font.size 14
        ]
        Element.none
"""


note : String
note =
    """
You will generate colors using a combination of 2 types, `Tone` and `Brightness`, and a function that's returns Palette.Color, `Palette.color`.

Example:

```elm
Palette.color Palette.toneDanger Palette.brightnessMiddle

-- or using a shorthand

Palette.danger
```

Once you hve a `Color` value, you have 2 ways to use it:


#### Transform into a `Element.Color`

This is the most common use case, as we use elm-ui.

````elm
Palette.color Palette.toneDanger Palette.brightnessMiddle
    |> Palette.toElementColor
    |> Background.color
````

#### Transform into a `String`

This is quite a specific use case and we try as much is possible avoid having to use `Html` in our codebase.

```elm
    Palette.color tonePrimary brightnessMiddle
        |> Palette.toCssColor
        |> Html.Attributes.style "font-color"
```

`UI.Palette` has 5 different tones (toneGray, tonePrimary, toneSuccess, toneWarning and toneDanger) and 5 different brightnesses (brightnessDarkest, brightnessMiddle, brightnessLight, brightnessLighter, brightnessLightest).

"""
        ++ goToDocsCallToAction "Palette"


hues : List ( String, Palette.Hue )
hues =
    [ ( "Gray", Palette.hueGray )
    , ( "Blue", Palette.hueBlue )
    , ( "Green", Palette.hueGreen )
    , ( "Yellow", Palette.hueYellow )
    , ( "Red", Palette.hueRed )
    ]


shades : List Palette.Shade
shades =
    [ Palette.shade800
    , Palette.shade700
    , Palette.shade600
    , Palette.shade500
    , Palette.shade400
    , Palette.shade300
    , Palette.shade200
    , Palette.shade100
    ]


colorView : Hue -> Palette.Shade -> Element msg
colorView hue shade =
    Element.el
        [ Palette.toBackgroundColor <| Palette.color hue shade
        , Element.width (px 100)
        , Element.height (px 100)
        , Element.spacing 10
        , Element.padding 10
        , Font.size 14
        ]
        Element.none


colorsView : Element msg
colorsView =
    hues
        |> List.map colorVarianceView
        |> Element.column
            [ Element.spacing 10
            ]


colorVarianceView : ( String, Hue ) -> Element msg
colorVarianceView ( label, hue ) =
    Element.row
        [ Element.spacing 20 ]
        [ shades
            |> List.map (colorView hue)
            |> Element.row []
        , Element.text label
        ]
