module Palette exposing (stories)

import Element exposing (Element, px)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import PluginOptions exposing (defaultWithoutMenu)
import UI.Palette.V2 as Palette exposing (Hue, Shade)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerUI, goToDocsCallToAction, prettifyElmCode)


renderConfig : RenderConfig
renderConfig =
    RenderConfig.init { width = 1920, height = 1080 } RenderConfig.localeEnglish


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


shades : List ( String, Palette.Shade )
shades =
    [ ( "800", Palette.shade800 )
    , ( "700", Palette.shade700 )
    , ( "600", Palette.shade600 )
    , ( "500", Palette.shade500 )
    , ( "400", Palette.shade400 )
    , ( "300", Palette.shade300 )
    , ( "200", Palette.shade200 )
    , ( "100", Palette.shade100 )
    ]


colorView : Hue -> ( String, Palette.Shade ) -> Element msg
colorView hue ( shadeLabel, shade ) =
    Element.column [ Element.spacing 8 ]
        [ Element.el
            [ Palette.toBackgroundColor <| Palette.color hue shade
            , Element.width (px 80)
            , Element.height (px 60)
            , Border.rounded 8
            ]
            Element.none
        , Text.subtitle2 shadeLabel
            |> Text.renderElement renderConfig
            |> Element.el [ Element.width (Element.px 70) ]
        ]


colorsView : Element msg
colorsView =
    hues
        |> List.map colorVarianceView
        |> Element.column
            [ Element.spacing 24
            ]


colorVarianceView : ( String, Hue ) -> Element msg
colorVarianceView ( label, hue ) =
    Element.row
        [ Element.spacing 20 ]
        [ Text.heading6 label
            |> Text.renderElement renderConfig
            |> Element.el [ Element.width (Element.px 70) ]
        , shades
            |> List.map (colorView hue)
            |> Element.row [ Element.spacing 8 ]
        ]
