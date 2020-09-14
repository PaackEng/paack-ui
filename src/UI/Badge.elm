module UI.Badge exposing
    ( Badge
    , grayLight, primaryLight, successLight, warningLight, dangerLight
    , grayDark, primaryDark, successDark, warningDark, dangerDark
    , renderElement
    , withTone
    )

{-| Badges are small elements displayed, usually on the right of texts or top-right corner of the view, serving as counters, tags, or labels.

Six color schemes are available: Primary, Warning, Danger, Success, Light, and Dark.
But, brightness can also variate using the palette's brightness values with [`Badge.withBrightness`](UI-Palette#withBrightness).

A badge can be created and rendered as in the following pipeline:

    Element.row []
        [ -- Some cool content
        , Badge.grayDark (String.fromInt (List.length someList))
            |> Badge.renderElement renderConfig
            |> Element.el [ Element.alignTop ]
        ]


# Building

@docs Badge


## Light

@docs grayLight, primaryLight, successLight, warningLight, dangerLight


## Dark

@docs grayDark, primaryDark, successDark, warningDark, dangerDark


# Rendering

@docs renderElement

-}

import Element exposing (Element, px, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import UI.Internal.Palette as Palette
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text


{-| The `Badge` type is used for describing the component for later rendering.
-}
type Badge
    = Badge Properties


type alias Properties =
    { content : String
    , tone : BadgeTone
    , brightness : BadgeBrightness
    }


type BadgeTone
    = ToneGray
    | TonePrimary
    | ToneDanger
    | ToneWarning
    | ToneSuccess


type BadgeBrightness
    = Light
    | Dark


{-| A light grayish variation of the badge.

    Badge.grayLight "EMPTY"

-}
grayLight : String -> Badge
grayLight content =
    Badge { content = content, tone = ToneGray, brightness = Light }


{-| A light primary-color variation of the badge.

    Badge.primaryLight "NEW"

-}
primaryLight : String -> Badge
primaryLight content =
    Badge { content = content, tone = TonePrimary, brightness = Light }


{-| A light variation of the badge with warning-tone.

    Badge.warningLight "0"

-}
warningLight : String -> Badge
warningLight content =
    Badge { content = content, tone = ToneWarning, brightness = Light }


{-| A light variation of the badge with danger-tone.

    Badge.dangerLight "ERROR"

-}
dangerLight : String -> Badge
dangerLight content =
    Badge { content = content, tone = ToneDanger, brightness = Light }


{-| A light variation of the badge with success-tone.

    Badge.successLight "SENT"

-}
successLight : String -> Badge
successLight content =
    Badge { content = content, tone = ToneSuccess, brightness = Light }


{-| A dark grayish variation of the badge.

    Badge.grayDark "EMPTY"

-}
grayDark : String -> Badge
grayDark content =
    Badge { content = content, tone = ToneGray, brightness = Dark }


{-| A primary-color variation of the badge.

    Badge.primaryDark "NEW"

-}
primaryDark : String -> Badge
primaryDark content =
    Badge { content = content, tone = TonePrimary, brightness = Dark }


{-| A variation of the badge with warning-tone.

    Badge.warningDark "0"

-}
warningDark : String -> Badge
warningDark content =
    Badge { content = content, tone = ToneWarning, brightness = Dark }


{-| A variation of the badge with danger-tone.

    Badge.dangerDark "ERROR"

-}
dangerDark : String -> Badge
dangerDark content =
    Badge { content = content, tone = ToneDanger, brightness = Dark }


{-| A variation of the badge with success-tone.

    Badge.successDark "SENT"

-}
successDark : String -> Badge
successDark content =
    Badge { content = content, tone = ToneSuccess, brightness = Dark }


withTone : (String -> Badge) -> Badge -> Badge
withTone builder (Badge { content }) =
    builder content



-- Render


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Badge -> Element msg
renderElement cfg (Badge { content, tone, brightness }) =
    let
        ( background, textColor ) =
            colors tone brightness
    in
    Text.overline content
        |> Text.withColor textColor
        |> Text.renderElement cfg
        |> Element.el
            [ Element.width shrink
            , Font.center
            , background
                |> Palette.toElementColor
                |> Background.color
            , Element.paddingEach { top = 4, bottom = 4, left = 7, right = 6 }
            , Element.height (px 20)
            , Border.rounded 10
            ]


colors : BadgeTone -> BadgeBrightness -> ( Palette.Color, Palette.Color )
colors tone brightness =
    case ( brightness, tone ) of
        ( Light, ToneGray ) ->
            ( Palette.color Palette.toneGray Palette.brightnessLighter
            , Palette.color Palette.toneGray Palette.brightnessDarkest
            )

        ( Light, TonePrimary ) ->
            ( Palette.color Palette.tonePrimary Palette.brightnessLightest
            , Palette.color Palette.tonePrimary Palette.brightnessDarkest
            )

        ( Light, ToneWarning ) ->
            ( Palette.color Palette.toneWarning Palette.brightnessLightest
            , Palette.color Palette.toneWarning Palette.brightnessDarkest
            )

        ( Light, ToneDanger ) ->
            ( Palette.color Palette.toneDanger Palette.brightnessLightest
            , Palette.color Palette.toneDanger Palette.brightnessDarkest
            )

        ( Light, ToneSuccess ) ->
            ( Palette.color Palette.toneSuccess Palette.brightnessLightest
            , Palette.color Palette.toneSuccess Palette.brightnessDarkest
            )

        ( Dark, ToneGray ) ->
            ( Palette.color Palette.toneGray Palette.brightnessDarkest
            , Palette.color Palette.toneGray Palette.brightnessLightest
            )

        ( Dark, TonePrimary ) ->
            ( Palette.color Palette.tonePrimary Palette.brightnessDarkest
            , Palette.color Palette.tonePrimary Palette.brightnessLightest
            )

        ( Dark, ToneWarning ) ->
            ( Palette.color Palette.toneDanger Palette.brightnessDarkest
            , Palette.color Palette.toneDanger Palette.brightnessLightest
            )

        ( Dark, ToneDanger ) ->
            ( Palette.color Palette.toneWarning Palette.brightnessDarkest
            , Palette.color Palette.toneWarning Palette.brightnessLightest
            )

        ( Dark, ToneSuccess ) ->
            ( Palette.color Palette.toneSuccess Palette.brightnessDarkest
            , Palette.color Palette.toneSuccess Palette.brightnessLightest
            )
