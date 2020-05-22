module UI.Alert exposing
    ( Alert
    , danger
    , light
    , primary
    , success
    , toEl
    , warning
    )

import Element exposing (Element)
import Element.Background as Background
import UI.Palette as Palette exposing (brightnessLighter, brightnessMiddle, toneDanger, toneGray, tonePrimary, toneSuccess, toneWarning)
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
    | ToneLight


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


light : String -> Alert msg
light title =
    Alert
        { title = title, tone = ToneLight }


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


getTextColor : AlertTone -> Palette.Color
getTextColor alertTone =
    alertTone
        |> getBackgroundColor
        |> Palette.withContrast True


getBackgroundColor : AlertTone -> Palette.Color
getBackgroundColor alertTone =
    case alertTone of
        ToneWarning ->
            Palette.color toneWarning brightnessMiddle

        ToneDanger ->
            Palette.color toneDanger brightnessMiddle

        TonePrimary ->
            Palette.color tonePrimary brightnessMiddle

        ToneSuccess ->
            Palette.color toneSuccess brightnessMiddle

        ToneLight ->
            Palette.color toneGray brightnessLighter
