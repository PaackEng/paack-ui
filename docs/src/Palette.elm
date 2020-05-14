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


lums : List Palette.Brightness
lums =
    [ Palette.lumDarkest
    , Palette.lumMiddle
    , Palette.lumLight
    , Palette.lumLighter
    , Palette.lumLightest
    ]


colorView : Tone -> Palette.Brightness -> Element msg
colorView tone brightness =
    Element.el
        [ Background.color <| Palette.toElColor ( tone, brightness )
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
        [ lums
            |> List.map (colorView tone)
            |> Element.row []
        , Element.text label
        ]