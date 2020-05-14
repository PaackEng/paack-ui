module UI.Badge exposing (Badge, danger, dark, light, primary, success, toEl, warning)

import Element exposing (Element, px, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
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
        ( bg, fg ) =
            toneToColors tone
    in
    Text.overline content
        |> Text.withColor fg
        |> Text.toEl cfg
        |> Element.el
            [ Element.width shrink
            , Font.center
            , Background.color bg
            , Element.paddingEach { top = 4, bottom = 4, left = 5, right = 3 }
            , Element.height (px 20)
            , Primitives.roundedBorders
            ]



-- Internal


toneToColors : BadgeTone -> ( Element.Color, TextColor )
toneToColors tone =
    case tone of
        ToneLight ->
            ( Palette.gray.lighter, Text.colorInverted )

        ToneDark ->
            ( Palette.gray.darkest, Text.colorBgMiddle )

        TonePrimary ->
            ( Palette.primary.light, Text.colorBgMiddle )

        ToneWarning ->
            ( Palette.warning.light, Text.colorBgMiddle )

        ToneDanger ->
            ( Palette.danger.light, Text.colorBgMiddle )

        ToneSuccess ->
            ( Palette.success.light, Text.colorInverted )
