module UI.Palette exposing
    ( Tone, toneGray, tonePrimary, toneSuccess, toneWarning, toneDanger
    , Brightness, brightnessDarkest, brightnessMiddle, brightnessLight, brightnessLighter, brightnessLightest
    , Color, color
    , setContrasting
    , withAlpha
    , toElementColor, toCssColor
    )

{-| `UI.Palette` is an interface offering all colors variations proposed in the design system.

    Palette.color tonePrimary brightnessMiddle
        |> Palette.toElementColor


# What color

@docs Tone, toneGray, tonePrimary, toneSuccess, toneWarning, toneDanger


# How much bright

@docs Brightness, brightnessDarkest, brightnessMiddle, brightnessLight, brightnessLighter, brightnessLightest


# Building

@docs Color, color


# Inverting

@docs setContrasting


# Making it transparent

@docs withAlpha


# Obtaining a usable variation

@docs toElementColor, toCssColor

-}

import Element
import UI.Internal.Basics exposing (ifThenElse)
import UI.Internal.Palette as Internal exposing (..)
import UI.Utils.Element exposing (colorSetOpacity)


{-| `Palette.Color` upholds data about some desired color.
-}
type Color
    = Color Properties Options


type alias Properties =
    { tone : Tone, brightness : Brightness }


type alias Options =
    { contrast : Bool
    , alpha : Float
    }


{-| The design system describes four main entries that here are called Tones.

A tone is a color with five brightness variations and a specific purpose.

The four tones are Gray, Primary, Success, Warning, and Danger.

-}
type Tone
    = ToneGray
    | TonePrimary
    | ToneSuccess
    | ToneDanger
    | ToneWarning


{-| Each [tone](UI-Palette#Tone) is later split in five brightness variations.

The five variations are Darkest, Middle, Light, Lighter, Lightest.

-}
type Brightness
    = BrightnessDarkest
    | BrightnessMiddle
    | BrightnessLight
    | BrightnessLighter
    | BrightnessLightest


primaryDark1 : Color
primaryDark1 =
    color TonePrimary BrightnessDarkest


primary : Color
primary =
    color TonePrimary BrightnessMiddle


primaryLight1 : Color
primaryLight1 =
    color TonePrimary BrightnessLight


primaryLight2 : Color
primaryLight2 =
    color TonePrimary BrightnessLighter


primaryLight3 : Color
primaryLight3 =
    color TonePrimary BrightnessLightest


successDark1 : Color
successDark1 =
    color ToneSuccess BrightnessDarkest


success : Color
success =
    color ToneSuccess BrightnessMiddle


successLight1 : Color
successLight1 =
    color ToneSuccess BrightnessLight


successLight2 : Color
successLight2 =
    color ToneSuccess BrightnessLighter


successLight3 : Color
successLight3 =
    color ToneSuccess BrightnessLightest


dangerDark1 : Color
dangerDark1 =
    color ToneDanger BrightnessDarkest


danger : Color
danger =
    color ToneDanger BrightnessMiddle


dangerLight1 : Color
dangerLight1 =
    color ToneDanger BrightnessLight


dangerLight2 : Color
dangerLight2 =
    color ToneDanger BrightnessLighter


dangerLight3 : Color
dangerLight3 =
    color ToneDanger BrightnessLightest


warningDark1 : Color
warningDark1 =
    color ToneWarning BrightnessDarkest


warning : Color
warning =
    color ToneWarning BrightnessMiddle


warningLight1 : Color
warningLight1 =
    color ToneWarning BrightnessLight


warningLight2 : Color
warningLight2 =
    color ToneWarning BrightnessLighter


warningLight3 : Color
warningLight3 =
    color ToneWarning BrightnessLightest


black : Color
black =
    color ToneGray BrightnessDarkest


gray : Color
gray =
    color ToneGray BrightnessMiddle


grayLight1 : Color
grayLight1 =
    color ToneGray BrightnessLight


grayLight2 : Color
grayLight2 =
    color ToneGray BrightnessLighter


grayLight3 : Color
grayLight3 =
    color ToneGray BrightnessLightest


{-| Given a tone and brightness, initiates the building of a color.

    Palette.color tonePrimary brightnessMiddle

-}
color : Tone -> Brightness -> Color
color tone brightness =
    Color (Properties tone brightness) defaultOptions


{-| Transforms a [`Palette.Color`](UI-Palette#Color) into an Elm-UI-compatible color.

    let
        backgroundColor =
            Palette.color tonePrimary brightnessMiddle
    in
    Element.el
        [ backgroundColor
            |> Palette.setContrasting True
            |> Palette.toElementColor
            |> Element.Font.color
        , backgroundColor
            |> Palette.toElementColor
            |> Element.Background.color
        ]
    <|
        Element.text "Hello World!"

-}
toElementColor : Color -> Element.Color
toElementColor (Color { tone, brightness } { alpha, contrast }) =
    tone
        |> ifThenElse contrast contrastColors toColors
        |> getBrightness brightness
        |> colorSetOpacity alpha


{-| Given some color, inverts it to contrast. Useful for contrasting text with the background.

    backgroundColor
        |> Palette.setContrasting True
        |> Palette.toElementColor
        |> Element.Font.color

-}
setContrasting : Bool -> Color -> Color
setContrasting enabled (Color prop opt) =
    Color prop { opt | contrast = enabled }


{-| Applies an alpha value to the color, making it transparent.

    backgroundColor
        |> Palette.withAlpha 0.5
        |> Palette.toElementColor
        |> Element.Background.color

-}
withAlpha : Float -> Color -> Color
withAlpha alpha (Color prop opt) =
    Color prop { opt | alpha = alpha }


{-| Transforms a [`Palette.Color`](UI-Palette#Color) into a CSS-compatible parameter.

    Palette.color tonePrimary brightnessMiddle
        |> Palette.toCssColor
        |> Html.Attributes.style "font-color"

-}
toCssColor : Color -> String
toCssColor data =
    toElementColor data
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


{-| The darkest variation of some tone.
-}
brightnessDarkest : Brightness
brightnessDarkest =
    BrightnessDarkest


{-| The first light variation of some tone.
-}
brightnessLight : Brightness
brightnessLight =
    BrightnessLight


{-| The increased-light variation of some tone.
-}
brightnessLighter : Brightness
brightnessLighter =
    BrightnessLighter


{-| The lightest variation of some tone.
-}
brightnessLightest : Brightness
brightnessLightest =
    BrightnessLightest


{-| The base of all variations of some tone.
-}
brightnessMiddle : Brightness
brightnessMiddle =
    BrightnessMiddle


{-| A redish tone.
-}
toneDanger : Tone
toneDanger =
    ToneDanger


{-| A grayish tone.
-}
toneGray : Tone
toneGray =
    ToneGray


{-| A blueish tone.
-}
tonePrimary : Tone
tonePrimary =
    TonePrimary


{-| A greenish tone.
-}
toneSuccess : Tone
toneSuccess =
    ToneSuccess


{-| A yellowish tone.
-}
toneWarning : Tone
toneWarning =
    ToneWarning



-- Internals


getBrightness : Brightness -> ToneColors -> Element.Color
getBrightness brightness =
    case brightness of
        BrightnessDarkest ->
            .darkest

        BrightnessMiddle ->
            .middle

        BrightnessLight ->
            .light

        BrightnessLighter ->
            .lighter

        BrightnessLightest ->
            .lightest


toColors : Tone -> ToneColors
toColors tone =
    case tone of
        ToneGray ->
            Internal.gray

        TonePrimary ->
            Internal.primary

        ToneSuccess ->
            Internal.success

        ToneDanger ->
            Internal.danger

        ToneWarning ->
            Internal.warning


contrastColors : Tone -> ToneColors
contrastColors tone =
    case tone of
        ToneGray ->
            darkConstrast

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
