module UI.Badge exposing (Badge, danger, dark, light, primary, success, toEl, warning)

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
    { tone : BadgeTone }


type BadgeTone
    = ToneLight
    | ToneDark
    | TonePrimary
    | ToneWarning
    | ToneDanger
    | ToneSuccess


light : String -> Badge
light content =
    Badge { content = content } { tone = ToneLight }


dark : String -> Badge
dark content =
    Badge { content = content } { tone = ToneDark }


primary : String -> Badge
primary content =
    Badge { content = content } { tone = TonePrimary }


warning : String -> Badge
warning content =
    Badge { content = content } { tone = ToneWarning }


danger : String -> Badge
danger content =
    Badge { content = content } { tone = ToneDanger }


success : String -> Badge
success content =
    Badge { content = content } { tone = ToneSuccess }



-- Render


toEl : RenderConfig -> Badge -> Element msg
toEl cfg (Badge { content } { tone }) =
    let
        bg =
            toneToColor tone

        fg =
            Palette.withContrast True bg
    in
    Text.overline content
        |> Text.withColor fg
        |> Text.toEl cfg
        |> Element.el
            [ Element.width shrink
            , Font.center
            , Background.color <| Palette.toElColor bg
            , Element.paddingEach { top = 4, bottom = 4, left = 5, right = 3 }
            , Element.height (px 20)
            , Primitives.roundedBorders
            ]



-- Internal


toneToColor : BadgeTone -> Palette.Color
toneToColor tone =
    case tone of
        ToneLight ->
            Palette.color Palette.toneGray Palette.lumLighter

        ToneDark ->
            Palette.color Palette.toneGray Palette.lumDarkest

        TonePrimary ->
            Palette.color Palette.tonePrimary Palette.lumLight

        ToneWarning ->
            Palette.color Palette.toneWarning Palette.lumLight

        ToneDanger ->
            Palette.color Palette.toneDanger Palette.lumLight

        ToneSuccess ->
            Palette.color Palette.toneSuccess Palette.lumLight
