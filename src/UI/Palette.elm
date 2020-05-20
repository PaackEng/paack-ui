module UI.Palette exposing
    ( Brightness
    , Color
    , Tone
    , color
    , lumDarkest
    , lumLight
    , lumLighter
    , lumLightest
    , lumMiddle
    , toCssColor
    , toElColor
    , toneDanger
    , toneGray
    , tonePrimary
    , toneSuccess
    , toneWarning
    , withAlpha
    , withContrast
    )

import Element
import UI.Internal.Basics exposing (ifThenElse)
import UI.Internal.Palette exposing (..)
import UI.Utils.Element exposing (colorWithOpacity)


type Color
    = Color Properties Options


type alias Properties =
    { tone : Tone, brightness : Brightness }


type alias Options =
    { contrast : Bool
    , alpha : Float
    }


type Tone
    = ToneGray
    | TonePrimary
    | ToneSuccess
    | ToneDanger
    | ToneWarning


type Brightness
    = LumDarkest
    | LumMiddle
    | LumLight
    | LumLighter
    | LumLightest


color : Tone -> Brightness -> Color
color tone brightness =
    Color (Properties tone brightness) defaultOptions


toElColor : Color -> Element.Color
toElColor (Color { tone, brightness } { alpha, contrast }) =
    tone
        |> ifThenElse contrast contrastColors toColors
        |> getLum brightness
        |> colorWithOpacity alpha


withContrast : Bool -> Color -> Color
withContrast enabled (Color prop opt) =
    Color prop { opt | contrast = enabled }


withAlpha : Float -> Color -> Color
withAlpha alpha (Color prop opt) =
    Color prop { opt | alpha = alpha }


toCssColor : Color -> String
toCssColor data =
    toElColor data
        |> Element.toRgb
        |> (\{ red, green, blue, alpha } ->
                "rgba("
                    ++ String.fromInt (ceiling (red * 255))
                    ++ ","
                    ++ String.fromInt (ceiling (green * 255))
                    ++ ","
                    ++ String.fromInt (ceiling (blue * 255))
                    ++ ","
                    ++ String.fromFloat alpha
                    ++ ")"
           )


lumDarkest : Brightness
lumDarkest =
    LumDarkest


lumLight : Brightness
lumLight =
    LumLight


lumLighter : Brightness
lumLighter =
    LumLighter


lumLightest : Brightness
lumLightest =
    LumLightest


lumMiddle : Brightness
lumMiddle =
    LumMiddle


toneDanger : Tone
toneDanger =
    ToneDanger


toneGray : Tone
toneGray =
    ToneGray


tonePrimary : Tone
tonePrimary =
    TonePrimary


toneSuccess : Tone
toneSuccess =
    ToneSuccess


toneWarning : Tone
toneWarning =
    ToneWarning



-- Internals


getLum : Brightness -> ToneColors -> Element.Color
getLum brightness =
    case brightness of
        LumDarkest ->
            .darkest

        LumMiddle ->
            .middle

        LumLight ->
            .light

        LumLighter ->
            .lighter

        LumLightest ->
            .lightest


toColors : Tone -> ToneColors
toColors tone =
    case tone of
        ToneGray ->
            gray

        TonePrimary ->
            primary

        ToneSuccess ->
            success

        ToneDanger ->
            danger

        ToneWarning ->
            warning


contrastColors : Tone -> ToneColors
contrastColors tone =
    case tone of
        ToneGray ->
            contrastGray

        TonePrimary ->
            contrastPrimary

        ToneSuccess ->
            contrastSuccess

        ToneDanger ->
            contrastDanger

        ToneWarning ->
            contrastWarning


defaultOptions : Options
defaultOptions =
    { alpha = 1
    , contrast = False
    }
