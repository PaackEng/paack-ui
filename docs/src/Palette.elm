module Palette exposing (stories)

import Element exposing (Element, px)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import UI.Palette as Palette exposing (Brightness, Tone)
import UIExplorer exposing (storiesOf)


stories =
    storiesOf
        "Colors"
        [ ( "Colors"
          , \_ -> Element.layout [] colorsView
          , { note = "" }
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
        [ Background.color <| Palette.toElColor <| Palette.color tone brightness
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
