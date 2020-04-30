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
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import UI.Button as Button exposing (Button)
import UI.Icons as Icons
import UI.Theme as Theme


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


toEl : ActionBar msg -> Element msg
toEl (ActionBar options) =
    row
        [ width fill
        , paddingEach
            { bottom = 20
            , left = 40
            , right = 20
            , top = 20
            }
        , Border.color Theme.gray4
        , Border.width 1
        , Background.color Theme.white
        , Theme.borderShadow
        , alignBottom
        ]
        [ viewTextContainer options
        , viewButtonsContainer options
        ]


viewTextContainer : Options msg -> Element msg
viewTextContainer options =
    column
        [ alignLeft
        , Theme.tinySpacing
        ]
        [ el [ Theme.title, Font.bold ] <| text options.title
        , el [ Theme.subtitle ] <| text options.subtitle
        ]


viewButtonsContainer : Options msg -> Element msg
viewButtonsContainer options =
    let
        closeBtn =
            case options.onClose of
                Just msg ->
                    {- TODO
                       Button.button msg
                           |> Button.withDangerColor
                           |> Button.withIcon (Icons.close "")
                           |> Button.toEl
                    -}
                    none

                Nothing ->
                    none

        buttons =
            {- TODO
               [ closeBtn ]
                   |> List.append (List.map Button.toEl options.buttons)
            -}
            [ none ]
    in
    row [ alignRight, Theme.smallSpacing ]
        buttons
