module UI.Internal.Text exposing (..)

import Element exposing (Attribute, Element, fill)
import Element.Font as Font
import List
import UI.Internal.Basics exposing (ifThenElse)
import UI.Internal.Palette as Palette
import UI.Palette as Palette exposing (brightnessDarkest, toneGray)
import UI.RenderConfig exposing (RenderConfig, isMobile)
import UI.Utils.Element as Element


type Text
    = Text (List Span) TextOptions


type Span
    = Span SpanProperties Options


type alias Options =
    { color : TextColor
    , oneLineEllipsis : Bool
    }


type alias SpanProperties =
    { content : String
    , size : TextSize
    }


type alias TextOptions =
    { ellipsis : Bool }


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
    | ColorForLightButtonDisabled
    | ColorInherit


defaultText : TextSize -> String -> Text
defaultText size content =
    Text [ Span (SpanProperties content size) spanDefaultOptions ] textDefaultOptions


spanDefaultOptions : Options
spanDefaultOptions =
    { color = defaultColor
    , oneLineEllipsis = False
    }


textDefaultOptions : TextOptions
textDefaultOptions =
    { ellipsis = False }


defaultColor : TextColor
defaultColor =
    ColorPalette <| Palette.color toneGray brightnessDarkest


fontColor : TextColor -> Maybe Element.Color
fontColor color =
    case color of
        ColorPalette paletteColor ->
            Just <| Palette.toElColor paletteColor

        ColorForLightButtonDisabled ->
            Just <| Palette.textLightButtonDisabled

        ColorInherit ->
            Nothing


attributes : RenderConfig -> TextSize -> Bool -> TextColor -> List (Attribute msg)
attributes config size ellipsis color =
    let
        mobile =
            isMobile config

        sizeAttrs =
            if mobile then
                mobileAttributes size

            else
                deskAttributes size

        ellipsisAttrs attrs =
            if ellipsis then
                (oneLineHeight mobile size :: Element.ellipsis) ++ attrs

            else
                attrs

        colorAttr attrs =
            case fontColor color of
                Just elColor ->
                    Font.color elColor :: attrs

                Nothing ->
                    attrs
    in
    sizeAttrs
        |> ellipsisAttrs
        |> colorAttr


deskAttributes : TextSize -> List (Attribute msg)
deskAttributes size =
    case size of
        SizeHeading1 ->
            [ Font.size 92
            , Font.letterSpacing -4.5
            , Font.semiBold
            ]

        SizeHeading2 ->
            [ Font.size 64
            , Font.letterSpacing -3
            , Font.semiBold
            ]

        SizeHeading3 ->
            [ Font.size 48
            , Font.letterSpacing -2
            , Font.semiBold
            ]

        SizeHeading4 ->
            [ Font.size 32
            , Font.letterSpacing -1
            , Font.semiBold
            ]

        SizeHeading5 ->
            [ Font.size 24
            , Font.letterSpacing 0
            , Font.semiBold
            ]

        SizeHeading6 ->
            [ Font.size 20
            , Font.letterSpacing 0.15
            , Font.semiBold
            ]

        SizeSubtitle1 ->
            [ Font.size 16
            , Font.letterSpacing 0.2
            , Font.medium
            ]

        SizeSubtitle2 ->
            [ Font.size 14
            , Font.letterSpacing 0.25
            , Font.semiBold
            ]

        SizeBody1 ->
            [ Font.size 16
            , Element.spacing 8
            , Font.letterSpacing 0.2
            , Font.regular
            ]

        SizeBody2 ->
            [ Font.size 14
            , Element.spacing 10
            , Font.letterSpacing 0.25
            , Font.regular
            ]

        SizeCaption ->
            [ Font.size 12
            , Element.spacing 4
            , Font.letterSpacing 0.5
            , Font.medium
            ]

        SizeOverline ->
            [ Font.size 10
            , Font.letterSpacing 2
            , Font.extraBold
            ]


mobileAttributes : TextSize -> List (Attribute msg)
mobileAttributes size =
    case size of
        SizeHeading1 ->
            [ Font.size 56
            , Font.letterSpacing -2
            , Font.semiBold
            ]

        SizeHeading2 ->
            [ Font.size 48
            , Font.letterSpacing -1.5
            , Font.semiBold
            ]

        SizeHeading3 ->
            [ Font.size 40
            , Font.letterSpacing -1
            , Font.semiBold
            ]

        SizeHeading4 ->
            [ Font.size 32
            , Font.letterSpacing -0.5
            , Font.semiBold
            ]

        SizeHeading5 ->
            [ Font.size 24
            , Font.letterSpacing 0
            , Font.semiBold
            ]

        SizeHeading6 ->
            [ Font.size 20
            , Font.letterSpacing 0.15
            , Font.semiBold
            ]

        SizeSubtitle1 ->
            [ Font.size 16
            , Font.letterSpacing 0.2
            , Font.medium
            ]

        SizeSubtitle2 ->
            [ Font.size 14
            , Font.letterSpacing 0.25
            , Font.semiBold
            ]

        SizeBody1 ->
            [ Font.size 16
            , Element.spacing 8
            , Font.letterSpacing 0.2
            , Font.regular
            ]

        SizeBody2 ->
            [ Font.size 14
            , Element.spacing 10
            , Font.letterSpacing 0.25
            , Font.regular
            ]

        SizeCaption ->
            [ Font.size 12
            , Element.spacing 4
            , Font.letterSpacing 0.5
            , Font.medium
            ]

        SizeOverline ->
            [ Font.size 10
            , Font.letterSpacing 2
            , Font.extraBold
            ]


oneLineHeight : Bool -> TextSize -> Attribute msg
oneLineHeight isMobile size =
    (Element.px >> Element.height) <|
        case size of
            SizeHeading1 ->
                ifThenElse isMobile 56 92

            SizeHeading2 ->
                ifThenElse isMobile 48 64

            SizeHeading3 ->
                ifThenElse isMobile 40 48

            SizeHeading4 ->
                32

            SizeHeading5 ->
                24

            SizeHeading6 ->
                20

            SizeSubtitle1 ->
                16

            SizeSubtitle2 ->
                14

            SizeBody1 ->
                16 + 8

            SizeBody2 ->
                14 + 10

            SizeCaption ->
                12 + 4

            SizeOverline ->
                -- TODO: Looks like OVERLINE has lineHeight
                10 + 6


mapOptions : (Options -> Options) -> Text -> Text
mapOptions applier (Text spans combo) =
    Text (List.map (spanMapOptions applier) spans) combo


spanMapOptions : (Options -> Options) -> Span -> Span
spanMapOptions applier (Span prop opt) =
    Span prop (applier opt)


spanRenderEl : RenderConfig -> Span -> Element msg
spanRenderEl cfg (Span { content, size } { color, oneLineEllipsis }) =
    content
        |> Element.text
        |> List.singleton
        |> Element.paragraph
            (attributes cfg size oneLineEllipsis color)


getSpans : Text -> List Span
getSpans (Text spans _) =
    spans


textMapOptions : (TextOptions -> TextOptions) -> Text -> Text
textMapOptions applier (Text spans combo) =
    Text spans (applier combo)


setEllipsis : Bool -> Text -> Text
setEllipsis val text =
    text
        |> mapOptions (\opt -> { opt | oneLineEllipsis = val })
        |> textMapOptions (\opt -> { opt | ellipsis = val })


combinedAttrs : TextOptions -> List (Attribute msg)
combinedAttrs { ellipsis } =
    if ellipsis then
        [ Element.width fill, Element.clipX ]

    else
        []
