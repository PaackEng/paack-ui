module UI.Badge exposing
    ( Badge
    , grayLight, primaryLight, successLight, warningLight, dangerLight
    , grayDark, primaryDark, successDark, warningDark, dangerDark
    , withTone
    , renderElement
    , outlineDark, outlineLight
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

import Dropdown exposing (Msg)
import Element exposing (Attr, Attribute, Element, px, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Button exposing (EmbossedTone(..))
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
    { withOutline : Bool
    , withIcon : Maybe Icon
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


{-| A grayscale variation of the badge with outline.

    Badge.outline "OUTLINE"

-}
outlineLight : String -> Badge
outlineLight content =
    Options True Nothing
        |> Badge { content = content, tone = ToneClear, brightness = Light }


outlineDark : String -> Badge
outlineDark content =
    Options True Nothing
        |> Badge { content = content, tone = ToneClear, brightness = Dark }


{-| A light grayish variation of the badge.

    Badge.grayLight "EMPTY"

-}
grayLight : String -> Badge
grayLight content =
    Options False Nothing
        |> Badge { content = content, tone = ToneGray, brightness = Light }


{-| A light primary-color variation of the badge.

    Badge.primaryLight "NEW"

-}
primaryLight : String -> Badge
primaryLight content =
    Options False Nothing
        |> Badge { content = content, tone = TonePrimary, brightness = Light }


{-| A light variation of the badge with warning-tone.

    Badge.warningLight "0"

-}
warningLight : String -> Badge
warningLight content =
    Options False Nothing
        |> Badge { content = content, tone = ToneWarning, brightness = Light }


{-| A light variation of the badge with danger-tone.

    Badge.dangerLight "ERROR"

-}
dangerLight : String -> Badge
dangerLight content =
    Options False Nothing
        |> Badge { content = content, tone = ToneDanger, brightness = Light }


{-| A light variation of the badge with success-tone.

    Badge.successLight "SENT"

-}
successLight : String -> Badge
successLight content =
    Options False Nothing
        |> Badge { content = content, tone = ToneSuccess, brightness = Light }


{-| A dark grayish variation of the badge.

    Badge.grayDark "EMPTY"

-}
grayDark : String -> Badge
grayDark content =
    Options False Nothing
        |> Badge { content = content, tone = ToneGray, brightness = Dark }


{-| A primary-color variation of the badge.

    Badge.primaryDark "NEW"

-}
primaryDark : String -> Badge
primaryDark content =
    Options False Nothing
        |> Badge { content = content, tone = TonePrimary, brightness = Dark }


{-| A variation of the badge with warning-tone.

    Badge.warningDark "0"

-}
warningDark : String -> Badge
warningDark content =
    Options False Nothing
        |> Badge { content = content, tone = ToneWarning, brightness = Dark }


{-| A variation of the badge with danger-tone.

    Badge.dangerDark "ERROR"

-}
dangerDark : String -> Badge
dangerDark content =
    Options False Nothing
        |> Badge { content = content, tone = ToneDanger, brightness = Dark }


{-| A variation of the badge with success-tone.

    Badge.successDark "SENT"

-}
successDark : String -> Badge
successDark content =
    Options False Nothing
        |> Badge { content = content, tone = ToneSuccess, brightness = Dark }


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
withTone builder (Badge { content } _) =
    builder content


companionIcon : RenderConfig -> Icon -> Element msg
companionIcon cfg icon =
    icon
        |> Icon.withCustomSize 8
        |> Icon.renderElement cfg



-- Render


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Badge -> Element msg
renderElement cfg (Badge { content, tone, brightness } { withIcon, withOutline }) =
    case withIcon of
        Just icon ->
            let
                ( background, textColor, outlineColor ) =
                    colors tone brightness
            in
            Element.column []
                [ companionIcon cfg icon
                , Text.subtitle2 content
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
                        , Element.paddingEach { top = 4, bottom = 6, left = 8, right = 8 }
                        , Element.height (px 24)
                        , Border.rounded 12
                        , Border.width 2
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
                    , Element.paddingEach { top = 4, bottom = 6, left = 8, right = 8 }
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
