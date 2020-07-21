module Palette exposing (stories)

import Element exposing (Element, px)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import PluginOptions exposing (defaultWithoutMenu)
import UI.Palette as Palette exposing (Brightness, Tone)
import UIExplorer exposing (storiesOf)
import Utils exposing (prettifyElmCode)


stories =
    storiesOf
        "Colors"
        [ ( "Colors"
          , \_ -> Element.layout [] colorsView
          , { defaultWithoutMenu
                | code =
                    prettifyElmCode
                        """
import UI.Palette as Palette

myRedSquare =
    Element.el
        [ Palette.color Palette.toneDanger Palette.brightnessMiddle
            |> Background.color
        , Element.width (px 100)
        , Element.height (px 100)
        , Element.spacing 10
        , Element.padding 10
        , Font.size 14
        ]
        Element.none
        """
                , note = """
You will generate colors using a combination of 2 types, `Tone` and `Brightness`, and a function that's returns Palette.Color, `Palette.color`.

Example:

```elm
Palette.color Palette.toneDanger Palette.brightnessMiddle
```

Once you hve a `Color` value, you have 2 ways to use it:


#### Transform into a `Element.Color`

This is the most common use case, as we use elm-ui.

````elm
Palette.color Palette.toneDanger Palette.brightnessMiddle
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

For more information, check the [docs](https://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-Palette)
"""
            }
          )
        ]


tones : List ( String, Palette.Tone )
tones =
    [ ( "Gray", Palette.toneGray )
    , ( "Primary", Palette.tonePrimary )
    , ( "Success", Palette.toneSuccess )
    , ( "Danger", Palette.toneDanger )
    , ( "Warning", Palette.toneWarning )
    ]


brightnesses : List Palette.Brightness
brightnesses =
    [ Palette.brightnessDarkest
    , Palette.brightnessMiddle
    , Palette.brightnessLight
    , Palette.brightnessLighter
    , Palette.brightnessLightest
    ]


colorView : Tone -> Palette.Brightness -> Element msg
colorView tone brightness =
    Element.el
        [ Background.color <| Palette.toElementColor <| Palette.color tone brightness
        , Element.width (px 100)
        , Element.height (px 100)
        , Element.spacing 10
        , Element.padding 10
        , Font.size 14
        ]
        Element.none


colorsView : Element msg
colorsView =
    tones
        |> List.map colorVarianceView
        |> Element.column
            [ Element.spacing 10
            ]


colorVarianceView : ( String, Tone ) -> Element msg
colorVarianceView ( label, tone ) =
    Element.row
        [ Element.spacing 20 ]
        [ brightnesses
            |> List.map (colorView tone)
            |> Element.row []
        , Element.text label
        ]
