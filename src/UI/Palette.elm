module UI.Palette exposing
    ( blue, gray, green, red, yellow
    , blue100, blue200, blue300, blue400, blue500, blue600, blue700, blue800
    , gray100, gray200, gray300, gray400, gray500, gray600, gray700, gray800
    , green800, red100, red200, red300, red400, red500, red600, red700, green100, green200, green300, green400, green500, green600, green700, red800, yellow100, yellow200, yellow300, yellow400, yellow500, yellow600, yellow700, yellow800
    , Hue, hueBlue, hueGray, hueGreen, hueRed, hueYellow
    , Shade, shade100, shade200, shade300, shade400, shade500, shade600, shade700, shade800
    , Color, color, toBackgroundColor, toFontColor, toBorderColor
    , genericBlack, genericWhite, genericSkyBlue
    , withAlpha
    , toElementColor, toCssColor
    )

{-| `UI.Palette` is an interface offering all colors variations proposed in the design system.

    Palette.blue600
        |> Palette.toElementColor


# Which color

@docs blue, gray, green, red, yellow
@docs blue100, blue200, blue300, blue400, blue500, blue600, blue700, blue800
@docs gray100, gray200, gray300, gray400, gray500, gray600, gray700, gray800
@docs green800, red100, red200, red300, red400, red500, red600, red700, green100, green200, green300, green400, green500, green600, green700, red800, yellow100, yellow200, yellow300, yellow400, yellow500, yellow600, yellow700, yellow800


# Which hue

@docs Hue, hueBlue, hueGray, hueGreen, hueRed, hueYellow


# Which shade

@docs Shade, shade100, shade200, shade300, shade400, shade500, shade600, shade700, shade800


# Building

@docs Color, color, toBackgroundColor, toFontColor, toBorderColor


# Generic colors

@docs genericBlack, genericWhite, genericSkyBlue


# Making it transparent

@docs withAlpha


# Obtaining a usable variation

@docs toElementColor, toCssColor

-}

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import UI.Internal.Colors as Internal
import UI.Utils.Element exposing (colorSetOpacity)


{-| `Palette.Color` holds data about some desired color.
-}
type Color
    = Color Properties Options
    | Generic GenericColor Options


type GenericColor
    = GenericBlack
    | GenericWhite
    | GenericSkyBlue


type alias Properties =
    { hue : Hue, shade : Shade }


type alias Options =
    { alpha : Float
    }


{-| The design system describes four main entries that here are called Hues.

A hue is one of the five pure colors of the palette.

-}
type Hue
    = HueGray
    | HueBlue
    | HueGreen
    | HueYellow
    | HueRed


{-| Each [hue](UI-Palette#Hue) can be paired with eight different shades.

The shades are 800, 700, 600, 500, 400, 300, 200 and 100.

-}
type Shade
    = Shade800
    | Shade700
    | Shade600
    | Shade500
    | Shade400
    | Shade300
    | Shade200
    | Shade100


{-| Given a hue and shade it constructs a color.

    Palette.color hueBlue shade600

-}
color : Hue -> Shade -> Color
color hue shade =
    Color (Properties hue shade) defaultOptions


{-| Black as in `#000`.

    Palette.genericBlack

-}
genericBlack : Color
genericBlack =
    Generic GenericBlack defaultOptions


{-| White as in `#FFF`.

    Palette.genericWhite

-}
genericWhite : Color
genericWhite =
    Generic GenericWhite defaultOptions


{-| That blue used in our visual identity.

    Palette.genericSkyBlue

-}
genericSkyBlue : Color
genericSkyBlue =
    Generic GenericSkyBlue defaultOptions


{-| Shorthand for setting background colors

    Element.row
        [ Palette.blue600 |> Palette.toBackgroundColor
        ]
        []

-}
toBackgroundColor : Color -> Element.Decoration
toBackgroundColor =
    toElementColor >> Background.color


{-| Shorthand for setting font colors

    Element.row
        [ Palette.blue600 |> Palette.toFontColor
        , Font.size 16
        , Font.justify
        ]
        []

-}
toFontColor : Color -> Element.Decoration
toFontColor =
    toElementColor >> Font.color


{-| Shorthand for setting border colors

    Element.row
        [ Palette.blue600 |> Palette.toBorderColor
        , Border.width 2
        , Border.solid
        ]
        []

-}
toBorderColor : Color -> Element.Decoration
toBorderColor =
    toElementColor >> Border.color


{-| Manually transforms a [`Palette.Color`](UI-Palette#Color) into an Elm-UI-compatible color.

    let
        backgroundColor =
            Palette.blue700
    in
    Element.el
        [ backgroundColor
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
toElementColor color_ =
    case color_ of
        Color { hue, shade } { alpha } ->
            toColor shade hue
                |> colorSetOpacity alpha

        Generic genericColor { alpha } ->
            (case genericColor of
                GenericBlack ->
                    Element.rgb 0 0 0

                GenericWhite ->
                    Element.rgb 1 1 1

                GenericSkyBlue ->
                    Element.rgb255 0 163 224
            )
                |> colorSetOpacity alpha


{-| Applies an alpha value to the color adding transparency.

    backgroundColor
        |> Palette.withAlpha 0.5
        |> Palette.toElementColor
        |> Element.Background.color

-}
withAlpha : Float -> Color -> Color
withAlpha alpha color_ =
    case color_ of
        Color prop opt ->
            Color prop { opt | alpha = alpha }

        Generic genericColor opt ->
            Generic genericColor { opt | alpha = alpha }


{-| Transforms a [`Palette.Color`](UI-Palette#Color) into a CSS-compatible parameter.

    Palette.blue700
        |> Palette.toCssColor
        |> Html.Attributes.style "font-color"

-}
toCssColor : Color -> String
toCssColor data =
    toElementColor data
        |> Element.toRgb
        |> (\rgb ->
                "rgba("
                    ++ String.fromInt (ceiling (rgb.red * 255))
                    ++ ","
                    ++ String.fromInt (ceiling (rgb.green * 255))
                    ++ ","
                    ++ String.fromInt (ceiling (rgb.blue * 255))
                    ++ ","
                    ++ String.fromFloat rgb.alpha
                    ++ ")"
           )


getShade : Shade -> Internal.Shades -> Element.Color
getShade shade =
    .background
        << (case shade of
                Shade800 ->
                    .shade800

                Shade700 ->
                    .shade700

                Shade600 ->
                    .shade600

                Shade500 ->
                    .shade500

                Shade400 ->
                    .shade400

                Shade300 ->
                    .shade300

                Shade200 ->
                    .shade200

                Shade100 ->
                    .shade100
           )


toColor : Shade -> Hue -> Element.Color
toColor shade hue =
    case hue of
        HueBlue ->
            getShade shade Internal.navyBlue

        HueRed ->
            getShade shade Internal.red

        HueGray ->
            getShade shade Internal.gray

        HueYellow ->
            getShade shade Internal.yellow

        HueGreen ->
            getShade shade Internal.green


defaultOptions : Options
defaultOptions =
    { alpha = 1
    }


{-| Shorthand for the most common occurrence of blue. Equivalent to blue700
-}
blue : Color
blue =
    blue700


{-| Shorthand for the most common occurrence of red. Equivalent to red600
-}
red : Color
red =
    red600


{-| Shorthand for the most common occurrence of gray. Equivalent to gray700
-}
gray : Color
gray =
    gray700


{-| Shorthand for the most common occurrence of yellow. Equivalent to yellow500
-}
yellow : Color
yellow =
    yellow500


{-| Shorthand for the most common occurrence of green. Equivalent to green500
-}
green : Color
green =
    green500


{-| -}
shade800 : Shade
shade800 =
    Shade800


{-| -}
shade700 : Shade
shade700 =
    Shade700


{-| -}
shade600 : Shade
shade600 =
    Shade600


{-| -}
shade500 : Shade
shade500 =
    Shade500


{-| -}
shade400 : Shade
shade400 =
    Shade400


{-| -}
shade300 : Shade
shade300 =
    Shade300


{-| -}
shade200 : Shade
shade200 =
    Shade200


{-| -}
shade100 : Shade
shade100 =
    Shade100


{-| -}
hueGray : Hue
hueGray =
    HueGray


{-| -}
hueBlue : Hue
hueBlue =
    HueBlue


{-| -}
hueGreen : Hue
hueGreen =
    HueGreen


{-| -}
hueYellow : Hue
hueYellow =
    HueYellow


{-| -}
hueRed : Hue
hueRed =
    HueRed


{-| -}
gray800 : Color
gray800 =
    color HueGray Shade800


{-| -}
gray700 : Color
gray700 =
    color HueGray Shade700


{-| -}
gray600 : Color
gray600 =
    color HueGray Shade600


{-| -}
gray500 : Color
gray500 =
    color HueGray Shade500


{-| -}
gray400 : Color
gray400 =
    color HueGray Shade400


{-| -}
gray300 : Color
gray300 =
    color HueGray Shade300


{-| -}
gray200 : Color
gray200 =
    color HueGray Shade200


{-| -}
gray100 : Color
gray100 =
    color HueGray Shade100


{-| -}
blue800 : Color
blue800 =
    color HueBlue Shade800


{-| -}
blue700 : Color
blue700 =
    color HueBlue Shade700


{-| -}
blue600 : Color
blue600 =
    color HueBlue Shade600


{-| -}
blue500 : Color
blue500 =
    color HueBlue Shade500


{-| -}
blue400 : Color
blue400 =
    color HueBlue Shade400


{-| -}
blue300 : Color
blue300 =
    color HueBlue Shade300


{-| -}
blue200 : Color
blue200 =
    color HueBlue Shade200


{-| -}
blue100 : Color
blue100 =
    color HueBlue Shade100


{-| -}
green800 : Color
green800 =
    color HueGreen Shade800


{-| -}
green700 : Color
green700 =
    color HueGreen Shade700


{-| -}
green600 : Color
green600 =
    color HueGreen Shade600


{-| -}
green500 : Color
green500 =
    color HueGreen Shade500


{-| -}
green400 : Color
green400 =
    color HueGreen Shade400


{-| -}
green300 : Color
green300 =
    color HueGreen Shade300


{-| -}
green200 : Color
green200 =
    color HueGreen Shade200


{-| -}
green100 : Color
green100 =
    color HueGreen Shade100


{-| -}
yellow800 : Color
yellow800 =
    color HueYellow Shade800


{-| -}
yellow700 : Color
yellow700 =
    color HueYellow Shade700


{-| -}
yellow600 : Color
yellow600 =
    color HueYellow Shade600


{-| -}
yellow500 : Color
yellow500 =
    color HueYellow Shade500


{-| -}
yellow400 : Color
yellow400 =
    color HueYellow Shade400


{-| -}
yellow300 : Color
yellow300 =
    color HueYellow Shade300


{-| -}
yellow200 : Color
yellow200 =
    color HueYellow Shade200


{-| -}
yellow100 : Color
yellow100 =
    color HueYellow Shade100


{-| -}
red800 : Color
red800 =
    color HueRed Shade800


{-| -}
red700 : Color
red700 =
    color HueRed Shade700


{-| -}
red600 : Color
red600 =
    color HueRed Shade600


{-| -}
red500 : Color
red500 =
    color HueRed Shade500


{-| -}
red400 : Color
red400 =
    color HueRed Shade400


{-| -}
red300 : Color
red300 =
    color HueRed Shade300


{-| -}
red200 : Color
red200 =
    color HueRed Shade200


{-| -}
red100 : Color
red100 =
    color HueRed Shade100
