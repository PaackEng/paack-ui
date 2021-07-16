module UI.Internal.Colors exposing (..)

import Element exposing (Attribute, rgb255)
import Element.Background as Background



-- Generics


black : Element.Color
black =
    rgb255 0 0 0


white : Element.Color
white =
    rgb255 0xFF 0xFF 0xFF


skyBlue : Element.Color
skyBlue =
    rgb255 0x00 0xA3 0xE0



-- Developers common


overlayBackground : Attribute msg
overlayBackground =
    Background.color (Element.rgba255 0 0 0 0.5)


mainBackground : Attribute msg
mainBackground =
    Background.color white



-- Shades baremetal


gray800 : Element.Color
gray800 =
    rgb255 0x1D 0x20 0x21


gray700 : Element.Color
gray700 =
    rgb255 0x3F 0x45 0x49


gray600 : Element.Color
gray600 =
    rgb255 0x73 0x7C 0x82


gray500 : Element.Color
gray500 =
    rgb255 0x9C 0xA5 0xAB


gray400 : Element.Color
gray400 =
    rgb255 0xC2 0xC8 0xCB


gray300 : Element.Color
gray300 =
    rgb255 0xDE 0xE1 0xE3


gray200 : Element.Color
gray200 =
    rgb255 0xF1 0xF2 0xF3


gray100 : Element.Color
gray100 =
    rgb255 0xF9 0xFA 0xFA


navyBlue800 : Element.Color
navyBlue800 =
    rgb255 0x00 0x26 0x3E


navyBlue700 : Element.Color
navyBlue700 =
    rgb255 0x19 0x4B 0x67


navyBlue600 : Element.Color
navyBlue600 =
    rgb255 0x4C 0x7B 0x99


navyBlue500 : Element.Color
navyBlue500 =
    rgb255 0x6F 0xA4 0xC3


navyBlue400 : Element.Color
navyBlue400 =
    rgb255 0x91 0xBD 0xD9


navyBlue300 : Element.Color
navyBlue300 =
    rgb255 0xBA 0xD6 0xE8


navyBlue200 : Element.Color
navyBlue200 =
    rgb255 0xD6 0xE9 0xF5


navyBlue100 : Element.Color
navyBlue100 =
    rgb255 0xEC 0xF3 0xF9


green800 : Element.Color
green800 =
    rgb255 0x06 0x4E 0x2B


green700 : Element.Color
green700 =
    rgb255 0x06 0x72 0x4B


green600 : Element.Color
green600 =
    rgb255 0x07 0xAB 0x66


green500 : Element.Color
green500 =
    rgb255 0x2A 0xCA 0x7E


green400 : Element.Color
green400 =
    rgb255 0x6E 0xE7 0x9E


green300 : Element.Color
green300 =
    rgb255 0xA7 0xF3 0xCA


green200 : Element.Color
green200 =
    rgb255 0xD1 0xFA 0xE1


green100 : Element.Color
green100 =
    rgb255 0xEC 0xFD 0xF5


yellow800 : Element.Color
yellow800 =
    rgb255 0x7B 0x44 0x03


yellow700 : Element.Color
yellow700 =
    rgb255 0xC2 0x6C 0x0A


yellow600 : Element.Color
yellow600 =
    rgb255 0xEB 0xA0 0x0A


yellow500 : Element.Color
yellow500 =
    rgb255 0xFB 0xC2 0x2D


yellow400 : Element.Color
yellow400 =
    rgb255 0xFC 0xD3 0x4D


yellow300 : Element.Color
yellow300 =
    rgb255 0xFD 0xE6 0x8A


yellow200 : Element.Color
yellow200 =
    rgb255 0xFE 0xF3 0xC7


yellow100 : Element.Color
yellow100 =
    rgb255 0xFF 0xFB 0xEB


red800 : Element.Color
red800 =
    rgb255 0x8C 0x10 0x10


red700 : Element.Color
red700 =
    rgb255 0xB9 0x25 0x1C


red600 : Element.Color
red600 =
    rgb255 0xDE 0x37 0x2C


red500 : Element.Color
red500 =
    rgb255 0xF6 0x5A 0x4F


red400 : Element.Color
red400 =
    rgb255 0xFF 0x87 0x81


red300 : Element.Color
red300 =
    rgb255 0xFB 0xBA 0xBA


red200 : Element.Color
red200 =
    rgb255 0xFE 0xE2 0xE2


red100 : Element.Color
red100 =
    rgb255 0xFE 0xF3 0xF2



-- Shades sum composition


type alias ColorPair =
    { background : Element.Color
    , text : Maybe Element.Color
    }


type alias Shades =
    { shade800 : ColorPair
    , shade700 : ColorPair
    , shade600 : ColorPair
    , shade500 : ColorPair
    , shade400 : ColorPair
    , shade300 : ColorPair
    , shade200 : ColorPair
    , shade100 : ColorPair
    }


gray : Shades
gray =
    { shade800 = ColorPair gray800 (Just white)
    , shade700 = ColorPair gray700 (Just white)
    , shade600 = ColorPair gray600 Nothing
    , shade500 = ColorPair gray500 (Just navyBlue800)
    , shade400 = ColorPair gray400 (Just navyBlue800)
    , shade300 = ColorPair gray300 (Just navyBlue800)
    , shade200 = ColorPair gray200 (Just navyBlue800)
    , shade100 = ColorPair gray100 (Just navyBlue800)
    }


navyBlue : Shades
navyBlue =
    { shade800 = ColorPair navyBlue800 (Just white)
    , shade700 = ColorPair navyBlue700 (Just white)
    , shade600 = ColorPair navyBlue600 Nothing
    , shade500 = ColorPair navyBlue500 (Just navyBlue800)
    , shade400 = ColorPair navyBlue400 (Just navyBlue800)
    , shade300 = ColorPair navyBlue300 (Just navyBlue800)
    , shade200 = ColorPair navyBlue200 (Just navyBlue800)
    , shade100 = ColorPair navyBlue100 (Just navyBlue800)
    }


green : Shades
green =
    { shade800 = ColorPair green800 (Just white)
    , shade700 = ColorPair green700 (Just white)
    , shade600 = ColorPair green600 Nothing
    , shade500 = ColorPair green500 (Just navyBlue800)
    , shade400 = ColorPair green400 (Just navyBlue800)
    , shade300 = ColorPair green300 (Just navyBlue800)
    , shade200 = ColorPair green200 (Just navyBlue800)
    , shade100 = ColorPair green100 (Just navyBlue800)
    }


yellow : Shades
yellow =
    { shade800 = ColorPair yellow800 (Just white)
    , shade700 = ColorPair yellow700 Nothing
    , shade600 = ColorPair yellow600 Nothing
    , shade500 = ColorPair yellow500 (Just navyBlue800)
    , shade400 = ColorPair yellow400 (Just navyBlue800)
    , shade300 = ColorPair yellow300 (Just navyBlue800)
    , shade200 = ColorPair yellow200 (Just navyBlue800)
    , shade100 = ColorPair yellow100 (Just navyBlue800)
    }


red : Shades
red =
    { shade800 = ColorPair red800 (Just white)
    , shade700 = ColorPair red700 (Just white)
    , shade600 = ColorPair red600 Nothing
    , shade500 = ColorPair red500 (Just navyBlue800)
    , shade400 = ColorPair red400 (Just navyBlue800)
    , shade300 = ColorPair red300 (Just navyBlue800)
    , shade200 = ColorPair red200 (Just navyBlue800)
    , shade100 = ColorPair red100 (Just navyBlue800)
    }



-- Shades prod composition


type Shade
    = Shade800
    | Shade700
    | Shade600
    | Shade500
    | Shade400
    | Shade300
    | Shade200
    | Shade100


allShades : List Shade
allShades =
    [ Shade800, Shade700, Shade600, Shade500, Shade400, Shade300, Shade200, Shade100 ]


type Hue
    = Gray
    | NavyBlue
    | Green
    | Yellow
    | Red


allTones : List Hue
allTones =
    [ Gray, NavyBlue, Green, Yellow, Red ]


colorFromShades : Shade -> Shades -> ColorPair
colorFromShades shade shades =
    case shade of
        Shade800 ->
            shades.shade800

        Shade700 ->
            shades.shade700

        Shade600 ->
            shades.shade600

        Shade500 ->
            shades.shade500

        Shade400 ->
            shades.shade400

        Shade300 ->
            shades.shade300

        Shade200 ->
            shades.shade200

        Shade100 ->
            shades.shade100


shadesFromTone : Hue -> Shades
shadesFromTone tone =
    case tone of
        Gray ->
            gray

        NavyBlue ->
            navyBlue

        Green ->
            green

        Yellow ->
            yellow

        Red ->
            red


getBackgroundColor : Hue -> Shade -> Element.Color
getBackgroundColor hue shade =
    shadesFromTone hue
        |> colorFromShades shade
        |> .background


getTextColor : Hue -> Shade -> Maybe Element.Color
getTextColor hue shade =
    shadesFromTone hue
        |> colorFromShades shade
        |> .text
