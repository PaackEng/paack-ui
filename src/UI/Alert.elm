module UI.Alert exposing
    ( Alert
    , error
    , info
    , success
    , toEl
    , warning
    , withCloseButton
    , withSubtitle
    , withTitle
    )

import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import ElementExtra exposing (when)
import UI.Attributes as ExtraAttrs
import UI.Icon as Icon
import UI.RenderConfig as RenderConfig
import UI.Theme as Theme


type Alert msg
    = Alert (Options msg)


type alias Options msg =
    { colorScheme : ColorScheme
    , title : String
    , subtitle : String
    , onCloseButtonClicked : Maybe msg
    }


type alias ColorScheme =
    { text : Color
    , background : Color
    }


defaultOptions : Options msg
defaultOptions =
    { colorScheme = { text = Theme.white, background = Theme.primary }
    , title = ""
    , subtitle = ""
    , onCloseButtonClicked = Nothing
    }


withTitle : String -> Alert msg -> Alert msg
withTitle title (Alert options) =
    Alert { options | title = title }


withSubtitle : String -> Alert msg -> Alert msg
withSubtitle subtitle (Alert options) =
    Alert { options | subtitle = subtitle }


withCloseButton : msg -> Alert msg -> Alert msg
withCloseButton msg (Alert options) =
    Alert { options | onCloseButtonClicked = Just msg }


info : Alert msg
info =
    Alert
        { defaultOptions
            | colorScheme = { text = Theme.white, background = Theme.primary }
        }


success : Alert msg
success =
    Alert
        { defaultOptions
            | colorScheme = { text = Theme.black, background = Theme.success }
        }


warning : Alert msg
warning =
    Alert
        { defaultOptions
            | colorScheme = { text = Theme.black, background = Theme.warning }
        }


error : Alert msg
error =
    Alert
        { defaultOptions
            | colorScheme = { text = Theme.white, background = Theme.error }
        }


toEl : Alert msg -> Element msg
toEl (Alert { title, subtitle, colorScheme, onCloseButtonClicked }) =
    row
        [ width fill
        , height shrink
        , Background.color colorScheme.background
        ]
        [ textView title subtitle colorScheme.text
        , closeBtn onCloseButtonClicked
        ]


dummyCfg =
    -- TODO: Remove
    RenderConfig.fromWindow { width = 192, height = 1080 }


closeBtn : Maybe msg -> Element msg
closeBtn maybeMsg =
    let
        btnView msg =
            el [ padding 6, alignRight ] <|
                el
                    [ Background.color Theme.blackShade
                    , paddingXY 10 10
                    , Theme.roundedBorder
                    , alignRight
                    , ExtraAttrs.ariaRole "button"
                    , onClick msg
                    , pointer
                    , Font.size 12
                    ]
                    (el [ Font.color Theme.white ] (Icon.close "Close" |> Icon.toEl dummyCfg))
    in
    maybeMsg
        |> Maybe.map btnView
        |> Maybe.withDefault none


textView : String -> String -> Color -> Element msg
textView title subtitle textColor =
    column
        [ Theme.tinySpacing
        , Font.color textColor
        , paddingXY Theme.sizings.large Theme.sizings.small
        , Theme.tinySpacing
        , height (fill |> minimum 45)
        ]
        [ column [ centerY ]
            [ el [ Theme.title ] <| text title
            , el [ Theme.subtitle ] <| text subtitle
            ]
        ]
