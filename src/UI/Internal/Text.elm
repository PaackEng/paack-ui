module UI.Internal.Text exposing (..)

import Element exposing (Attribute, Element)
import Element.Font as Font
import List
import UI.Internal.Palette as Palette
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig, isMobile)


type alias Options =
    { color : TextColor }


type alias Properties =
    { content : String
    , size : TextSize
    }


type Text
    = Text Properties Options


type TextSize
    = SizeHeading1
    | SizeHeading2
    | SizeHeading3
    | SizeHeading4
    | SizeHeading5
    | SizeHeading6
    | SizeSubtitle1
    | SizeSubtitle2
    | SizeBody1
    | SizeBody2
    | SizeCaption
    | SizeOverline


type TextColor
    = ColorPalette Palette.Color
    | ColorBgMiddle
    | ColorBgLightest
    | ColorBgLighter
    | ColorBgDisabled
    | ColorInherit


defaultText : TextSize -> String -> Text
defaultText size content =
    ( Palette.toneGray, Palette.lumDarkest )
        |> ColorPalette
        |> Options
        |> Text (Properties content size)


fontColor : TextColor -> Maybe Element.Color
fontColor color =
    case color of
        ColorPalette paletteColor ->
            Just <| Palette.toElColor paletteColor

        ColorBgMiddle ->
            Just <| Palette.textBgMiddle

        ColorBgLightest ->
            Just <| Palette.textBgLightest

        ColorBgLighter ->
            Just <| Palette.textBgLighter

        ColorBgDisabled ->
            Just <| Palette.textComponentDisabled

        ColorInherit ->
            Nothing


attributes : RenderConfig -> TextSize -> TextColor -> List (Attribute msg)
attributes config size color =
    let
        sizeAttrs =
            if isMobile config then
                mobileAttributes size

            else
                deskAttributes size
    in
    case fontColor color of
        Just attr ->
            Font.color attr :: sizeAttrs

        Nothing ->
            sizeAttrs


deskAttributes : TextSize -> List (Attribute msg)
deskAttributes size =
    case size of
        SizeHeading1 ->
            [ Font.size 92
            , Font.letterSpacing -4.5
            , Font.heavy
            ]

        SizeHeading2 ->
            [ Font.size 64
            , Font.letterSpacing -3
            , Font.heavy
            ]

        SizeHeading3 ->
            [ Font.size 48
            , Font.letterSpacing -2
            , Font.heavy
            ]

        SizeHeading4 ->
            [ Font.size 32
            , Font.letterSpacing -1
            , Font.heavy
            ]

        SizeHeading5 ->
            [ Font.size 24
            , Font.letterSpacing 0
            , Font.heavy
            ]

        SizeHeading6 ->
            [ Font.size 20
            , Font.letterSpacing 0.15
            , Font.heavy
            ]

        SizeSubtitle1 ->
            [ Font.size 16
            , Font.letterSpacing 0.2
            ]

        SizeSubtitle2 ->
            [ Font.size 14
            , Font.letterSpacing 0.25
            ]

        SizeBody1 ->
            [ Font.size 16
            , Element.spacing 8
            , Font.letterSpacing 0.2
            ]

        SizeBody2 ->
            [ Font.size 14
            , Element.spacing 10
            , Font.letterSpacing 0.25
            ]

        SizeCaption ->
            [ Font.size 12
            , Element.spacing 4
            , Font.letterSpacing 0.5
            ]

        SizeOverline ->
            [ Font.size 10
            , Font.letterSpacing 2
            , Font.heavy
            ]


mobileAttributes : TextSize -> List (Attribute msg)
mobileAttributes size =
    case size of
        SizeHeading1 ->
            [ Font.size 56
            , Font.letterSpacing -2
            ]

        SizeHeading2 ->
            [ Font.size 48
            , Font.letterSpacing -1.5
            ]

        SizeHeading3 ->
            [ Font.size 40
            , Font.letterSpacing -1
            ]

        SizeHeading4 ->
            [ Font.size 32
            , Font.letterSpacing -0.5
            ]

        SizeHeading5 ->
            [ Font.size 24
            , Font.letterSpacing 0
            ]

        SizeHeading6 ->
            [ Font.size 20
            , Font.letterSpacing 0.15
            ]

        SizeSubtitle1 ->
            [ Font.size 16
            , Font.letterSpacing 0.2
            ]

        SizeSubtitle2 ->
            [ Font.size 14
            , Font.letterSpacing 0.25
            ]

        SizeBody1 ->
            [ Font.size 16
            , Element.spacing 8
            , Font.letterSpacing 0.2
            ]

        SizeBody2 ->
            [ Font.size 14
            , Element.spacing 10
            , Font.letterSpacing 0.25
            ]

        SizeCaption ->
            [ Font.size 12
            , Element.spacing 4
            , Font.letterSpacing 0.5
            ]

        SizeOverline ->
            [ Font.size 10
            , Font.letterSpacing 2
            ]
