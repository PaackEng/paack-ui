module UI.ActionBar exposing
    ( ActionBar
    , actionBar
    , toEl
    , withButtons
    , withCloseButton
    , withSubtitle
    , withTitle
    )

import Element exposing (..)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import UI.Button as Button exposing (Button)
import UI.Icon as Icon
import UI.Internal.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.ARIA as ARIA


type alias Options msg =
    { title : String
    , subtitle : String
    , buttons : List (Button msg)
    , onClose : Maybe msg
    }


type ActionBar msg
    = ActionBar (Options msg)


actionBar : ActionBar msg
actionBar =
    ActionBar
        { title = ""
        , subtitle = ""
        , onClose = Nothing
        , buttons = []
        }


withCloseButton : msg -> ActionBar msg -> ActionBar msg
withCloseButton onClose (ActionBar options) =
    ActionBar
        { options
            | onClose = Just onClose
        }


withTitle : String -> ActionBar msg -> ActionBar msg
withTitle title (ActionBar options) =
    ActionBar
        { options
            | title = title
        }


withSubtitle : String -> ActionBar msg -> ActionBar msg
withSubtitle subtitle (ActionBar options) =
    ActionBar
        { options
            | subtitle = subtitle
        }


withButtons : List (Button msg) -> ActionBar msg -> ActionBar msg
withButtons buttons (ActionBar options) =
    ActionBar
        { options
            | buttons = buttons
        }


toEl : RenderConfig -> ActionBar msg -> Element msg
toEl cfg (ActionBar options) =
    row
        [ width fill
        , paddingEach
            { bottom = 12
            , left = 40
            , right = 16
            , top = 12
            }
        , Border.color Palette.gray.lighter
        , Border.width 1
        , alignBottom
        ]
        [ row [ width fill, height (shrink |> minimum 48) ]
            [ viewTextContainer cfg options
            , viewButtonsContainer cfg options
            ]
        ]


viewTextContainer : RenderConfig -> Options msg -> Element msg
viewTextContainer cfg options =
    column
        [ alignLeft
        , width fill
        ]
        [ Text.heading6 options.title
            |> Text.toEl cfg
        , Text.caption options.subtitle
            |> Text.toEl cfg
        ]


viewButtonsContainer : RenderConfig -> Options msg -> Element msg
viewButtonsContainer cfg options =
    let
        closeBtn msg =
            Icon.close "Close"
                |> Icon.toEl cfg
                |> Element.el
                    [ Element.pointer
                    , ARIA.roleAttr ARIA.roleButton
                    , ARIA.labelAttr "Close"
                    , Font.color Palette.danger.middle
                    , Events.onClick msg
                    ]

        customBtns =
            List.map (Button.toEl cfg) options.buttons

        buttons =
            case options.onClose of
                Just msg ->
                    customBtns ++ [ closeBtn msg ]

                Nothing ->
                    customBtns
    in
    row [ alignRight, width shrink, spacing 12 ]
        buttons
