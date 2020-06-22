module UI.Alert exposing
    ( Alert, primary, success, warning, danger
    , renderElement
    )

{-| The `UI.Alert` is a component for displaying feedback in a full-width banner.

It can have different background colors depending on the type of feedback.

An alert can be created and render as in the following pipeline:

    Element.column []
        [ Alert.danger "Failed to login."
            |> Alert.renderElement renderConfig
        , -- The rest of the screen (...)
        ]


# Building

@docs Alert, primary, success, warning, danger


# Rendering

@docs renderElement

-}

import Element exposing (Element)
import Element.Background as Background
import UI.Palette as Palette exposing (brightnessMiddle, toneDanger, tonePrimary, toneSuccess, toneWarning)
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text


type alias Properties =
    { title : String
    , tone : AlertTone
    }


{-| The `Alert msg` type is used for describing the component for later rendering.
-}
type Alert msg
    = Alert Properties


type AlertTone
    = ToneDanger
    | ToneWarning
    | TonePrimary
    | ToneSuccess


{-| The primary color scheme applied to the alert background.
-}
primary : String -> Alert msg
primary title =
    Alert
        { title = title, tone = TonePrimary }


{-| The success color scheme applied to the alert background.
-}
success : String -> Alert msg
success title =
    Alert
        { title = title, tone = ToneSuccess }


{-| The warning color scheme applied to the alert background.
-}
warning : String -> Alert msg
warning title =
    Alert
        { title = title, tone = ToneWarning }


{-| The danger color scheme applied to the alert background.
-}
danger : String -> Alert msg
danger title =
    Alert
        { title = title, tone = ToneDanger }


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Alert msg -> Element msg
renderElement cfg (Alert { title, tone }) =
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
            |> Palette.toElementColor
            |> Background.color
        , Element.alignTop
        ]
        [ Text.subtitle2 title
            |> Text.withColor (getTextColor tone)
            |> Text.renderElement cfg
        ]



-- Internals


getTextColor : AlertTone -> Palette.Color
getTextColor alertTone =
    alertTone
        |> getBackgroundColor
        |> Palette.setContrasting True


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
