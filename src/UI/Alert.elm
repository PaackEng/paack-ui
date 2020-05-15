module UI.Alert exposing
    ( Alert
    , danger
    , primary
    , success
    , toEl
    , warning
    )

import Element exposing (Element)
import Element.Background as Background
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text


type alias Properties =
    { title : String
    , tone : AlertTone
    }


type Alert msg
    = Alert Properties


type AlertTone
    = ToneDanger
    | ToneWarning
    | TonePrimary
    | ToneSuccess


primary : String -> Alert msg
primary title =
    Alert
        { title = title, tone = TonePrimary }


success : String -> Alert msg
success title =
    Alert
        { title = title, tone = ToneSuccess }


warning : String -> Alert msg
warning title =
    Alert
        { title = title, tone = ToneWarning }


danger : String -> Alert msg
danger title =
    Alert
        { title = title, tone = ToneDanger }


toEl : RenderConfig -> Alert msg -> Element msg
toEl cfg (Alert { title, tone }) =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.shrink
        , Element.paddingEach
            { top = 12
            , left = 20
            , right = 0
            , bottom = 12
            }
        , tone
            |> getBackgroundColor
            |> Palette.toElColor
            |> Background.color
        , Element.alignTop
        ]
        [ Text.subtitle2 title
            |> Text.withColor (getTextColor tone)
            |> Text.toEl cfg
        ]



-- Internals


getTextColor : AlertTone -> Text.TextColor
getTextColor alertTone =
    case alertTone of
        ToneWarning ->
            Text.colorInverted

        ToneDanger ->
            Text.colorBgMiddle

        TonePrimary ->
            Text.colorBgMiddle

        ToneSuccess ->
            Text.colorInverted


getBackgroundColor : AlertTone -> Palette.Color
getBackgroundColor alertTone =
    let
        colorTone =
            case alertTone of
                ToneWarning ->
                    Palette.toneWarning

                ToneDanger ->
                    Palette.toneDanger

                TonePrimary ->
                    Palette.tonePrimary

                ToneSuccess ->
                    Palette.toneSuccess
    in
    Palette.color colorTone Palette.lumMiddle
