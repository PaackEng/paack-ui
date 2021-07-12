module UI.Palette exposing
    ( primaryDark1, primary, primaryLight1, primaryLight2, primaryLight3, primaryLight4
    , successDark1, success, successLight1, successLight2, successLight3, successLight4
    , dangerDark1, danger, dangerLight1, dangerLight2, dangerLight3, dangerLight4
    , warningDark1, warning, warningLight1, warningLight2, warningLight3, warningLight4
    , black, gray, grayLight1, grayLight2, grayLight3, grayLight4
    , Tone, toneGray, tonePrimary, toneSuccess, toneWarning, toneDanger
    , Brightness, brightnessDarkest, brightnessMiddle, brightnessLight, brightnessLighter, brightnessLightest, brightnessLight4
    , Color, color
    , setContrasting
    , withAlpha
    , toElementColor, toCssColor
    )

{-| `UI.Palette` is an interface offering all colors variations proposed in the design system.

    Palette.color tonePrimary brightnessMiddle
        |> Palette.toElementColor


# What color

@docs primaryDark1, primary, primaryLight1, primaryLight2, primaryLight3, primaryLight4
@docs successDark1, success, successLight1, successLight2, successLight3, successLight4
@docs dangerDark1, danger, dangerLight1, dangerLight2, dangerLight3, dangerLight4
@docs warningDark1, warning, warningLight1, warningLight2, warningLight3, warningLight4
@docs black, gray, grayLight1, grayLight2, grayLight3, grayLight4


# What tone

@docs Tone, toneGray, tonePrimary, toneSuccess, toneWarning, toneDanger


# How much bright

@docs Brightness, brightnessDarkest, brightnessMiddle, brightnessLight, brightnessLighter, brightnessLightest, brightnessLight4


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
    = BrightnessDark1
    | BrightnessMiddle
    | BrightnessLight1
    | BrightnessLight2
    | BrightnessLight3
    | BrightnessLight4


{-| Shorthand for the primary tone with dark brightness
-}
primaryDark1 : Color
primaryDark1 =
    color TonePrimary BrightnessDark1


{-| Shorthand for the default primary tone
-}
primary : Color
primary =
    color TonePrimary BrightnessMiddle


{-| Shorthand for the primary tone with light brightness
-}
primaryLight1 : Color
primaryLight1 =
    color TonePrimary BrightnessLight1


{-| Shorthand for the primary tone with light brightness
-}
primaryLight2 : Color
primaryLight2 =
    color TonePrimary BrightnessLight2


{-| Shorthand for the primary tone with light brightness
-}
primaryLight3 : Color
primaryLight3 =
    color TonePrimary BrightnessLight3


{-| Shorthand for the primary tone with light brightness
-}
primaryLight4 : Color
primaryLight4 =
    color TonePrimary BrightnessLight4


{-| Shorthand for the success tone with dark brightness
-}
successDark1 : Color
successDark1 =
    color ToneSuccess BrightnessDark1


{-| Shorthand for the default success tone
-}
success : Color
success =
    color ToneSuccess BrightnessMiddle


{-| Shorthand for the success tone with light brightness
-}
successLight1 : Color
successLight1 =
    color ToneSuccess BrightnessLight1


{-| Shorthand for the success tone with light brightness
-}
successLight2 : Color
successLight2 =
    color ToneSuccess BrightnessLight2


{-| Shorthand for the success tone with light brightness
-}
successLight3 : Color
successLight3 =
    color ToneSuccess BrightnessLight3


{-| Shorthand for the success tone with light brightness
-}
successLight4 : Color
successLight4 =
    color ToneSuccess BrightnessLight4


{-| Shorthand for the success danger tone with dark brightness
-}
dangerDark1 : Color
dangerDark1 =
    color ToneDanger BrightnessDark1


{-| Shorthand for the default danger tone
-}
danger : Color
danger =
    color ToneDanger BrightnessMiddle


{-| Shorthand for the danger tone with light brightness
-}
dangerLight1 : Color
dangerLight1 =
    color ToneDanger BrightnessLight1


{-| Shorthand for the danger tone with light brightness
-}
dangerLight2 : Color
dangerLight2 =
    color ToneDanger BrightnessLight2


{-| Shorthand for the danger tone with light brightness
-}
dangerLight3 : Color
dangerLight3 =
    color ToneDanger BrightnessLight3


{-| Shorthand for the danger tone with light brightness
-}
dangerLight4 : Color
dangerLight4 =
    color ToneDanger BrightnessLight4


{-| Shorthand for the danger warning with dark brightness
-}
warningDark1 : Color
warningDark1 =
    color ToneWarning BrightnessDark1


{-| Shorthand for the default warning
-}
warning : Color
warning =
    color ToneWarning BrightnessMiddle


{-| Shorthand for the warning with light brightness
-}
warningLight1 : Color
warningLight1 =
    color ToneWarning BrightnessLight1


{-| Shorthand for the warning with light brightness
-}
warningLight2 : Color
warningLight2 =
    color ToneWarning BrightnessLight2


{-| Shorthand for the warning with light brightness
-}
warningLight3 : Color
warningLight3 =
    color ToneWarning BrightnessLight3


{-| Shorthand for the warning with light brightness
-}
warningLight4 : Color
warningLight4 =
    color ToneWarning BrightnessLight4


{-| Shorthand for the gray tone with dark brightness
-}
black : Color
black =
    color ToneGray BrightnessDark1


{-| Shorthand for the default gray tone
-}
gray : Color
gray =
    color ToneGray BrightnessMiddle


{-| Shorthand for the gray tone with light brightness
-}
grayLight1 : Color
grayLight1 =
    color ToneGray BrightnessLight1


{-| Shorthand for the gray tone with light brightness
-}
grayLight2 : Color
grayLight2 =
    color ToneGray BrightnessLight2


{-| Shorthand for the gray tone with light brightness
-}
grayLight3 : Color
grayLight3 =
    color ToneGray BrightnessLight3


{-| Shorthand for the gray tone with light brightness
-}
grayLight4 : Color
grayLight4 =
    color ToneGray BrightnessLight4


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
    backportColorV2 tone brightness contrast
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
    BrightnessDark1


{-| The first light variation of some tone.
-}
brightnessLight : Brightness
brightnessLight =
    BrightnessLight1


{-| The increased-light variation of some tone.
-}
brightnessLighter : Brightness
brightnessLighter =
    BrightnessLight2


{-| The greatly increased-light variation of some tone.
-}
brightnessLightest : Brightness
brightnessLightest =
    BrightnessLight3


{-| The lightest variation of some tone.
-}
brightnessLight4 : Brightness
brightnessLight4 =
    BrightnessLight4


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


defaultOptions : Options
defaultOptions =
    { alpha = 1
    , contrast = False
    }


backportColorV2 : Tone -> Brightness -> Bool -> Element.Color
backportColorV2 tone brightness contrast =
    let
        mimicContrast { text, background } =
            if contrast then
                Maybe.withDefault Internal.black text

            else
                background
    in
    mimicContrast <|
        case ( tone, brightness ) of
            ( ToneGray, BrightnessDark1 ) ->
                Internal.gray.shade800

            ( ToneGray, BrightnessMiddle ) ->
                Internal.gray.shade700

            ( ToneGray, BrightnessLight1 ) ->
                Internal.gray.shade600

            ( ToneGray, BrightnessLight2 ) ->
                Internal.gray.shade300

            ( ToneGray, BrightnessLight3 ) ->
                Internal.gray.shade200

            ( ToneGray, BrightnessLight4 ) ->
                Internal.gray.shade100

            ( TonePrimary, BrightnessDark1 ) ->
                Internal.navyBlue.shade800

            ( TonePrimary, BrightnessMiddle ) ->
                Internal.navyBlue.shade700

            ( TonePrimary, BrightnessLight1 ) ->
                Internal.navyBlue.shade600

            ( TonePrimary, BrightnessLight2 ) ->
                Internal.navyBlue.shade400

            ( TonePrimary, BrightnessLight3 ) ->
                Internal.navyBlue.shade200

            ( TonePrimary, BrightnessLight4 ) ->
                Internal.navyBlue.shade100

            ( ToneSuccess, BrightnessDark1 ) ->
                Internal.green.shade700

            ( ToneSuccess, BrightnessMiddle ) ->
                Internal.green.shade500

            ( ToneSuccess, BrightnessLight1 ) ->
                Internal.green.shade400

            ( ToneSuccess, BrightnessLight2 ) ->
                Internal.green.shade300

            ( ToneSuccess, BrightnessLight3 ) ->
                Internal.green.shade200

            ( ToneSuccess, BrightnessLight4 ) ->
                Internal.green.shade100

            ( ToneWarning, BrightnessDark1 ) ->
                Internal.yellow.shade800

            ( ToneWarning, BrightnessMiddle ) ->
                Internal.yellow.shade500

            ( ToneWarning, BrightnessLight1 ) ->
                Internal.yellow.shade400

            ( ToneWarning, BrightnessLight2 ) ->
                Internal.yellow.shade300

            ( ToneWarning, BrightnessLight3 ) ->
                Internal.yellow.shade200

            ( ToneWarning, BrightnessLight4 ) ->
                Internal.yellow.shade100

            ( ToneDanger, BrightnessDark1 ) ->
                Internal.red.shade700

            ( ToneDanger, BrightnessMiddle ) ->
                Internal.red.shade600

            ( ToneDanger, BrightnessLight1 ) ->
                Internal.red.shade500

            ( ToneDanger, BrightnessLight2 ) ->
                Internal.red.shade400

            ( ToneDanger, BrightnessLight3 ) ->
                Internal.red.shade200

            ( ToneDanger, BrightnessLight4 ) ->
                Internal.red.shade100
