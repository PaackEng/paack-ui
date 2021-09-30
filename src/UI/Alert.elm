module UI.Alert exposing
    ( Alert, primary, success, warning, danger
    , withGenericIcon
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


# Optional

@docs withGenericIcon


# Rendering

@docs renderElement

-}

import Element exposing (Element, fill, px)
import Element.Background as Background
import UI.Icon as Icon
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text


type alias Properties =
    { title : String
    , tone : AlertTone
    }


type alias Options =
    { genericIcon : Bool
    }


{-| The `Alert msg` type is used for describing the component for later rendering.
-}
type Alert msg
    = Alert Properties Options


type AlertTone
    = ToneDanger
    | ToneWarning
    | TonePrimary
    | ToneSuccess


{-| The primary color scheme applied to the alert background.

    Alert.primary "Proceed to login..."

-}
primary : String -> Alert msg
primary title =
    Alert
        { title = title, tone = TonePrimary }
        defaultOptions


{-| The success color scheme applied to the alert background.

    Alert.success "Category created with success."

-}
success : String -> Alert msg
success title =
    Alert
        { title = title, tone = ToneSuccess }
        defaultOptions


{-| The warning color scheme applied to the alert background.

    Alert.warning "Proceed with caution!"

-}
warning : String -> Alert msg
warning title =
    Alert
        { title = title, tone = ToneWarning }
        defaultOptions


{-| The danger color scheme applied to the alert background.

    Alert.danger "Failed to apply changes!"

-}
danger : String -> Alert msg
danger title =
    Alert
        { title = title, tone = ToneDanger }
        defaultOptions


{-| Displays an icon in the right side of the alert, depending on its color scheme.

    - Primary: Won't show any icon;
    - Success: Shows [UI.Icon.check](UI-Icon#check);
    - Warning: Will show a loading spinner;
    - Danger: Shows [UI.Icon.warning](UI-Icon#warning).

-}
withGenericIcon : Alert msg -> Alert msg
withGenericIcon (Alert prop opt) =
    Alert prop { opt | genericIcon = True }


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Alert msg -> Element msg
renderElement cfg (Alert { title, tone } { genericIcon }) =
    let
        color =
            getTextColor tone
    in
    Element.row
        [ Element.width fill
        , Element.height (px 40)
        , Element.paddingEach
            { top = 12
            , left = 20
            , right = 12
            , bottom = 12
            }
        , tone
            |> getBackgroundColor
            |> Palette.toElementColor
            |> Background.color
        , Element.alignTop
        ]
        [ Text.subtitle2 title
            |> Text.withColor color
            |> Text.renderElement cfg
            |> Element.el [ Element.centerY, Element.width fill ]
        , if genericIcon then
            icon cfg tone color

          else
            Element.none
        ]



-- Internals


defaultOptions : Options
defaultOptions =
    { genericIcon = False }


getTextColor : AlertTone -> Palette.Color
getTextColor alertTone =
    case alertTone of
        ToneWarning ->
            Palette.genericBlack

        ToneDanger ->
            Palette.genericWhite

        TonePrimary ->
            Palette.genericWhite

        ToneSuccess ->
            Palette.genericBlack


getBackgroundColor : AlertTone -> Palette.Color
getBackgroundColor alertTone =
    case alertTone of
        ToneWarning ->
            Palette.yellow500

        ToneDanger ->
            Palette.red700

        TonePrimary ->
            Palette.blue700

        ToneSuccess ->
            Palette.green500


icon : RenderConfig -> AlertTone -> Palette.Color -> Element msg
icon renderConfig alertTone color =
    let
        genericIcon iconFn =
            iconFn ""
                |> Icon.withSize Size.extraSmall
                |> Icon.withColor color
                |> Icon.renderElement renderConfig
    in
    case alertTone of
        ToneWarning ->
            genericIcon Icon.loader

        ToneDanger ->
            genericIcon Icon.warning

        TonePrimary ->
            Element.none

        ToneSuccess ->
            genericIcon Icon.check
