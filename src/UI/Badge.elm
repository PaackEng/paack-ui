module UI.Badge exposing (Badge, danger, dark, light, primary, renderElement, success, warning, withBrightness)

import Element exposing (Element, px, shrink)
import Element.Background as Background
import Element.Font as Font
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Palette as Palette exposing (brightnessDarkest, brightnessLight, brightnessLighter, toneDanger, toneGray, tonePrimary, toneSuccess, toneWarning)
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text


type Badge
    = Badge Properties Options


type alias Properties =
    { content : String
    , tone : Palette.Tone
    }


type alias Options =
    { brightness : Palette.Brightness
    }


light : String -> Badge
light content =
    Badge { content = content, tone = toneGray } { defaultOptions | brightness = brightnessLighter }


dark : String -> Badge
dark content =
    Badge { content = content, tone = toneGray } { defaultOptions | brightness = brightnessDarkest }


primary : String -> Badge
primary content =
    Badge { content = content, tone = tonePrimary } defaultOptions


warning : String -> Badge
warning content =
    Badge { content = content, tone = toneWarning } defaultOptions


danger : String -> Badge
danger content =
    Badge { content = content, tone = toneDanger } defaultOptions


success : String -> Badge
success content =
    Badge { content = content, tone = toneSuccess } defaultOptions


withBrightness : Palette.Brightness -> Badge -> Badge
withBrightness brightness (Badge prop opt) =
    Badge prop { opt | brightness = brightness }



-- Render


renderElement : RenderConfig -> Badge -> Element msg
renderElement cfg (Badge { content, tone } { brightness }) =
    let
        background =
            Palette.color tone brightness

        textColor =
            Palette.setContrasting True background
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
            , Element.paddingEach { top = 4, bottom = 4, left = 5, right = 3 }
            , Element.height (px 20)
            , Primitives.roundedBorders
            ]



-- Internal


defaultOptions : Options
defaultOptions =
    { brightness = brightnessLight
    }
