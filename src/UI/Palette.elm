module UI.Palette exposing (Brightness, Color, Tone, color, lumDarkest, lumLight, lumLighter, lumLightest, lumMiddle, toElColor, toneDanger, toneGray, tonePrimary, toneSuccess, toneWarning)

import Element
import UI.Internal.Palette exposing (..)


type alias Color =
    ( Tone, Brightness )


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
    ( tone, brightness )


toElColor : Color -> Element.Color
toElColor ( tone, brightness ) =
    tone
        |> toColors
        |> getLum brightness


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
