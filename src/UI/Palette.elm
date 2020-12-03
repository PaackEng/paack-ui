module UI.Palette exposing
    ( primaryDark1, primary, primaryLight1, primaryLight2, primaryLight3
    , successDark1, success, successLight1, successLight2, successLight3
    , dangerDark1, danger, dangerLight1, dangerLight2, dangerLight3
    , warningDark1, warning, warningLight1, warningLight2, warningLight3
    , black, gray, grayLight1, grayLight2, grayLight3
    , Tone, toneGray, tonePrimary, toneSuccess, toneWarning, toneDanger
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

@docs primaryDark1, primary, primaryLight1, primaryLight2, primaryLight3
@docs successDark1, success, successLight1, successLight2, successLight3
@docs dangerDark1, danger, dangerLight1, dangerLight2, dangerLight3
@docs warningDark1, warning, warningLight1, warningLight2, warningLight3
@docs black, gray, grayLight1, grayLight2, grayLight3


# What tone

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
import UI.Internal.Colors as Internal exposing (..)
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


{-| Shorthand for the primary tone with dark brightness
-}
primaryDark1 : Color
primaryDark1 =
    color TonePrimary BrightnessDarkest


{-| Shorthand for the default primary tone
-}
primary : Color
primary =
    color TonePrimary BrightnessMiddle


{-| Shorthand for the primary tone with light brightness
-}
primaryLight1 : Color
primaryLight1 =
    color TonePrimary BrightnessLight


{-| Shorthand for the primary tone with light brightness
-}
primaryLight2 : Color
primaryLight2 =
    color TonePrimary BrightnessLighter


{-| Shorthand for the primary tone with light brightness
-}
primaryLight3 : Color
primaryLight3 =
    color TonePrimary BrightnessLightest


{-| Shorthand for the success tone with dark brightness
-}
successDark1 : Color
successDark1 =
    color ToneSuccess BrightnessDarkest


{-| Shorthand for the default success tone
-}
success : Color
success =
    color ToneSuccess BrightnessMiddle


{-| Shorthand for the success tone with light brightness
-}
successLight1 : Color
successLight1 =
    color ToneSuccess BrightnessLight


{-| Shorthand for the success tone with light brightness
-}
successLight2 : Color
successLight2 =
    color ToneSuccess BrightnessLighter


{-| Shorthand for the success tone with light brightness
-}
successLight3 : Color
successLight3 =
    color ToneSuccess BrightnessLightest


{-| Shorthand for the success danger tone with dark brightness
-}
dangerDark1 : Color
dangerDark1 =
    color ToneDanger BrightnessDarkest


{-| Shorthand for the default danger tone
-}
danger : Color
danger =
    color ToneDanger BrightnessMiddle


{-| Shorthand for the danger tone with light brightness
-}
dangerLight1 : Color
dangerLight1 =
    color ToneDanger BrightnessLight


{-| Shorthand for the danger tone with light brightness
-}
dangerLight2 : Color
dangerLight2 =
    color ToneDanger BrightnessLighter


{-| Shorthand for the danger tone with light brightness
-}
dangerLight3 : Color
dangerLight3 =
    color ToneDanger BrightnessLightest


{-| Shorthand for the danger warning with dark brightness
-}
warningDark1 : Color
warningDark1 =
    color ToneWarning BrightnessDarkest


{-| Shorthand for the default warning
-}
warning : Color
warning =
    color ToneWarning BrightnessMiddle


{-| Shorthand for the warning with light brightness
-}
warningLight1 : Color
warningLight1 =
    color ToneWarning BrightnessLight


{-| Shorthand for the warning with light brightness
-}
warningLight2 : Color
warningLight2 =
    color ToneWarning BrightnessLighter


{-| Shorthand for the warning with light brightness
-}
warningLight3 : Color
warningLight3 =
    color ToneWarning BrightnessLightest


{-| Shorthand for the gray tone with dark brightness
-}
black : Color
black =
    color ToneGray BrightnessDarkest


{-| Shorthand for the default gray tone
-}
gray : Color
gray =
    color ToneGray BrightnessMiddle


{-| Shorthand for the gray tone with light brightness
-}
grayLight1 : Color
grayLight1 =
    color ToneGray BrightnessLight


{-| Shorthand for the gray tone with light brightness
-}
grayLight2 : Color
grayLight2 =
    color ToneGray BrightnessLighter


{-| Shorthand for the gray tone with light brightness
-}
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
