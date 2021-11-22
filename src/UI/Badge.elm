module UI.Badge exposing
    ( Badge
    , grayLight, primaryLight, successLight, warningLight, dangerLight
    , grayDark, primaryDark, successDark, warningDark, dangerDark
    , withTone
    , renderElement
    , outlineDark, outlineLight, withIcon
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


# Options

@docs withTone


# Rendering

@docs renderElement

-}

import Element exposing (Element, px, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import UI.Icon as Icon exposing (Icon)
import UI.Palette as Palette exposing (Color)
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text


{-| The `Badge` type is used for describing the component for later rendering.
-}
type Badge
    = Badge Properties Options


type alias Properties =
    { content : String
    , tone : BadgeTone
    , brightness : BadgeBrightness
    }


type alias Options =
    { icon : Maybe Icon
    }


type BadgeTone
    = ToneGray
    | TonePrimary
    | ToneDanger
    | ToneWarning
    | ToneSuccess
    | ToneClear


type BadgeBrightness
    = Light
    | Dark


defaultOptions : Options
defaultOptions =
    Options Nothing


{-| A variation of the badge with an icon.

    Badge.primaryLight "NEW"
        |> Badge.withIcon (Icon.car "Car")

-}
withIcon : Icon -> Badge -> Badge
withIcon icon (Badge properties options) =
    Badge properties { options | icon = Just icon }


{-| A variation of the badge with outline for light backgrounds.

    Badge.outlineLight "SENT"

-}
outlineLight : String -> Badge
outlineLight content =
    Badge { content = content, tone = ToneClear, brightness = Light } defaultOptions


{-| A light grayish variation of the badge.

    Badge.grayLight "EMPTY"

-}
grayLight : String -> Badge
grayLight content =
    Badge { content = content, tone = ToneGray, brightness = Light } defaultOptions


{-| A light primary-color variation of the badge.

    Badge.primaryLight "NEW"

-}
primaryLight : String -> Badge
primaryLight content =
    Badge { content = content, tone = TonePrimary, brightness = Light } defaultOptions


{-| A light variation of the badge with warning-tone.

    Badge.warningLight "0"

-}
warningLight : String -> Badge
warningLight content =
    Badge { content = content, tone = ToneWarning, brightness = Light } defaultOptions


{-| A light variation of the badge with danger-tone.

    Badge.dangerLight "ERROR"

-}
dangerLight : String -> Badge
dangerLight content =
    Badge { content = content, tone = ToneDanger, brightness = Light } defaultOptions


{-| A light variation of the badge with success-tone.

    Badge.successLight "SENT"

-}
successLight : String -> Badge
successLight content =
    Badge { content = content, tone = ToneSuccess, brightness = Light } defaultOptions


{-| A dark grayish variation of the badge.

    Badge.grayDark "EMPTY"

-}
grayDark : String -> Badge
grayDark content =
    Badge { content = content, tone = ToneGray, brightness = Dark } defaultOptions


{-| A primary-color variation of the badge.

    Badge.primaryDark "NEW"

-}
primaryDark : String -> Badge
primaryDark content =
    Badge { content = content, tone = TonePrimary, brightness = Dark } defaultOptions


{-| A variation of the badge with warning-tone.

    Badge.warningDark "0"

-}
warningDark : String -> Badge
warningDark content =
    Badge { content = content, tone = ToneWarning, brightness = Dark } defaultOptions


{-| A variation of the badge with danger-tone.

    Badge.dangerDark "ERROR"

-}
dangerDark : String -> Badge
dangerDark content =
    Badge { content = content, tone = ToneDanger, brightness = Dark } defaultOptions


{-| A variation of the badge with success-tone.

    Badge.successDark "SENT"

-}
successDark : String -> Badge
successDark content =
    Badge { content = content, tone = ToneSuccess, brightness = Dark } defaultOptions


{-| A variation of the badge with outline for dark backgrounds.

    Badge.outlineDark "SENT"

-}
outlineDark : String -> Badge
outlineDark content =
    Badge { content = content, tone = ToneClear, brightness = Dark } defaultOptions


{-| Replaces the tone of a badge with a new one.

    Badge.successLight "status"
        |> (if model.requestStatus == Model.Failed then
                Badge.withTone Badge.dangerLight

            else
                identity
           )
        |> Badge.renderElement renderConfig

-}
withTone : (String -> Badge) -> Badge -> Badge
withTone builder (Badge { content } originalOptions) =
   let (Badge newProps _) = builder content
   in (Badge newProps originalOptions)
    builder content


companionIcon : RenderConfig -> Icon -> Color -> Element msg
companionIcon cfg icon iconColor =
    icon
        |> Icon.withCustomSize 16
        |> Icon.withColor iconColor
        |> Icon.renderElement cfg


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Badge -> Element msg
renderElement cfg (Badge { content, tone, brightness } { icon }) =
    case icon of
        Just i ->
            let
                ( background, textColor, outlineColor ) =
                    colors tone brightness
            in
            Element.row
                [ Element.width shrink
                , Font.center
                , background
                    |> Palette.toElementColor
                    |> Background.color
                , outlineColor
                    |> Palette.toElementColor
                    |> Border.color
                , Element.height (px 24)
                , Border.rounded 12
                , Border.width 2
                ]
                [ Element.column
                    [ Element.paddingEach { top = 2, bottom = 2, left = 3, right = 2 }
                    ]
                    [ companionIcon cfg i textColor ]
                , Element.column
                    [ Element.paddingEach { top = 4, bottom = 0, left = 0, right = 6 }
                    ]
                    [ Text.subtitle2 content
                        |> Text.withColor textColor
                        |> Text.withOverflow Text.ellipsize
                        |> Text.renderElement cfg
                    ]
                ]

        Nothing ->
            let
                ( background, textColor, outlineColor ) =
                    colors tone brightness
            in
            Text.subtitle2 content
                |> Text.withColor textColor
                |> Text.withOverflow Text.ellipsize
                |> Text.renderElement cfg
                |> Element.el
                    [ Element.width shrink
                    , Font.center
                    , background
                        |> Palette.toElementColor
                        |> Background.color
                    , outlineColor
                        |> Palette.toElementColor
                        |> Border.color
                    , Element.paddingEach { top = 4, bottom = 0, left = 6, right = 6 }
                    , Element.height (px 24)
                    , Border.rounded 12
                    , Border.width 2
                    ]


colors : BadgeTone -> BadgeBrightness -> ( Palette.Color, Palette.Color, Palette.Color )
colors tone brightness =
    case ( brightness, tone ) of
        ( Light, ToneGray ) ->
            ( Palette.gray300
            , Palette.gray800
            , Palette.gray300
            )

        ( Light, TonePrimary ) ->
            ( Palette.blue200
            , Palette.blue800
            , Palette.blue200
            )

        ( Light, ToneWarning ) ->
            ( Palette.yellow200
            , Palette.yellow800
            , Palette.yellow200
            )

        ( Light, ToneDanger ) ->
            ( Palette.red200
            , Palette.red800
            , Palette.red200
            )

        ( Light, ToneSuccess ) ->
            ( Palette.green200
            , Palette.green800
            , Palette.green200
            )

        ( Light, ToneClear ) ->
            ( Palette.withAlpha 0 Palette.genericBlack
            , Palette.gray700
            , Palette.gray400
            )

        ( Dark, ToneGray ) ->
            ( Palette.gray800
            , Palette.gray200
            , Palette.gray800
            )

        ( Dark, TonePrimary ) ->
            ( Palette.blue800
            , Palette.blue200
            , Palette.blue800
            )

        ( Dark, ToneWarning ) ->
            ( Palette.yellow800
            , Palette.yellow200
            , Palette.yellow800
            )

        ( Dark, ToneDanger ) ->
            ( Palette.red800
            , Palette.red200
            , Palette.red800
            )

        ( Dark, ToneSuccess ) ->
            ( Palette.green800
            , Palette.green200
            , Palette.green800
            )

        ( Dark, ToneClear ) ->
            ( Palette.withAlpha 0 Palette.genericBlack
            , Palette.gray200
            , Palette.gray200
            )
