module UI.Badge exposing (Badge, danger, dark, light, primary, success, toEl, warning, withAlpha)

import Element exposing (Element, px, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text exposing (TextColor)


type Badge
    = Badge Properties Options


type alias Properties =
    { content : String }


type alias Options =
    { tone : BadgeTone
    , alpha : Float
    }


type BadgeTone
    = ToneLight
    | ToneDark
    | TonePrimary
    | ToneWarning
    | ToneDanger
    | ToneSuccess


light : String -> Badge
light content =
    Badge { content = content } { defaultOptions | tone = ToneLight }


dark : String -> Badge
dark content =
    Badge { content = content } { defaultOptions | tone = ToneDark }


primary : String -> Badge
primary content =
    Badge { content = content } { defaultOptions | tone = TonePrimary }


warning : String -> Badge
warning content =
    Badge { content = content } { defaultOptions | tone = ToneWarning }


danger : String -> Badge
danger content =
    Badge { content = content } { defaultOptions | tone = ToneDanger }


success : String -> Badge
success content =
    Badge { content = content } { defaultOptions | tone = ToneSuccess }


withAlpha : Float -> Badge -> Badge
withAlpha alpha (Badge prop opt) =
    Badge prop { opt | alpha = alpha }



-- Render


toEl : RenderConfig -> Badge -> Element msg
toEl cfg (Badge { content } { tone, alpha }) =
    let
        background =
            toneToColor tone

        textColor =
            Palette.withContrast True background
    in
    Text.overline content
        |> Text.withColor textColor
        |> Text.toEl cfg
        |> Element.el
            [ Element.width shrink
            , Font.center
            , background
                |> Palette.withAlpha alpha
                |> Palette.toElColor
                |> Background.color
            , Element.paddingEach { top = 4, bottom = 4, left = 5, right = 3 }
            , Element.height (px 20)
            , Primitives.roundedBorders
            ]



-- Internal


defaultOptions : Options
defaultOptions =
    { tone = TonePrimary
    , alpha = 1
    }


toneToColor : BadgeTone -> Palette.Color
toneToColor tone =
    case tone of
        ToneLight ->
            Palette.color Palette.toneGray Palette.brightnessLighter

        ToneDark ->
            Palette.color Palette.toneGray Palette.brightnessDarkest

        TonePrimary ->
            Palette.color Palette.tonePrimary Palette.brightnessLight

        ToneWarning ->
            Palette.color Palette.toneWarning Palette.brightnessLight

        ToneDanger ->
            Palette.color Palette.toneDanger Palette.brightnessLight

        ToneSuccess ->
            Palette.color Palette.toneSuccess Palette.brightnessLight
