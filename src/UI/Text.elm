module UI.Text exposing
    ( Text
    , TextBrightness
    , body1
    , body2
    , caption
    , heading1
    , heading2
    , heading3
    , heading4
    , heading5
    , heading6
    , lumBright
    , lumBrightest
    , lumNormal
    , overline
    , subtitle1
    , subtitle2
    , toEl
    , withBrightness
    )

import Element exposing (Attribute, Element)
import Element.Font as Font
import List
import UI.Internal.Palette as Palette
import UI.RenderConfig exposing (RenderConfig, isMobile)


type alias Options =
    { brightness : TextBrightness }


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


type TextBrightness
    = LumNormal
    | LumBright
    | LumBrightest


heading1 : String -> Text
heading1 content =
    defaultText SizeHeading1 content


heading2 : String -> Text
heading2 content =
    defaultText SizeHeading2 content


heading3 : String -> Text
heading3 content =
    defaultText SizeHeading3 content


heading4 : String -> Text
heading4 content =
    defaultText SizeHeading4 content


heading5 : String -> Text
heading5 content =
    defaultText SizeHeading5 content


heading6 : String -> Text
heading6 content =
    defaultText SizeHeading6 content


subtitle1 : String -> Text
subtitle1 content =
    defaultText SizeSubtitle1 content


subtitle2 : String -> Text
subtitle2 content =
    defaultText SizeSubtitle2 content


body1 : String -> Text
body1 content =
    defaultText SizeBody1 content


body2 : String -> Text
body2 content =
    defaultText SizeBody2 content


caption : String -> Text
caption content =
    defaultText SizeCaption content


overline : String -> Text
overline content =
    defaultText SizeOverline content


lumBright : TextBrightness
lumBright =
    LumBright


lumBrightest : TextBrightness
lumBrightest =
    LumBrightest


lumNormal : TextBrightness
lumNormal =
    LumNormal


withBrightness : TextBrightness -> Text -> Text
withBrightness brightness (Text prop opt) =
    Text prop { opt | brightness = brightness }


toEl : RenderConfig -> Text -> Element msg
toEl cfg text =
    case text of
        Text { content, size } { brightness } ->
            content
                |> Element.text
                |> List.singleton
                |> Element.paragraph (attributes cfg size brightness)



-- Internal


defaultText : TextSize -> String -> Text
defaultText size content =
    Text
        (Properties content size)
        (Options LumNormal)


attributes : RenderConfig -> TextSize -> TextBrightness -> List (Attribute msg)
attributes config size brightness =
    let
        fontColor =
            Font.color <|
                case brightness of
                    LumNormal ->
                        Palette.gray.darkest

                    LumBright ->
                        Palette.gray.middle

                    LumBrightest ->
                        Palette.gray.light

        sizeAttrs =
            if isMobile config then
                mobileAttributes size

            else
                deskAttributes size
    in
    fontColor :: sizeAttrs


deskAttributes : TextSize -> List (Attribute msg)
deskAttributes size =
    case size of
        SizeHeading1 ->
            [ Font.size 92
            , Font.letterSpacing -4.5
            ]

        SizeHeading2 ->
            [ Font.size 64
            , Font.letterSpacing -3
            ]

        SizeHeading3 ->
            [ Font.size 48
            , Font.letterSpacing -2
            ]

        SizeHeading4 ->
            [ Font.size 32
            , Font.letterSpacing -1
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
