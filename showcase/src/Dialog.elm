module Dialog exposing (stories)

import Element
import Msg
import PluginOptions exposing (defaultWithoutMenu)
import UI.Button as Button exposing (Button)
import UI.Icon as Icon
import UI.Internal.Palette exposing (overlayBackground)
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.V2.Dialog as Dialog
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, iconsSvgSprite, prettifyElmCode, story)


stories : RenderConfig -> ExplorerUI
stories cfg =
    storiesOf
        "Dialog"
        [ dialog cfg
        ]


dialog : RenderConfig -> ExplorerStory
dialog cfg =
    story
        ( "Dialog"
        , Element.column
            [ Element.centerX
            , Element.centerY
            , Element.padding 32
            , overlayBackground
            ]
            [ iconsSvgSprite
            , Dialog.Dialog
                { title = "Dialog Header"
                , icon = Icon.warning "Warning dialog"
                }
                { buttons = buttons
                , body = "I am body of the dialog" |> Text.body2 |> Text.renderElement cfg
                }
                |> Dialog.renderElement cfg
            ]
        , { defaultWithoutMenu | code = code }
        )


buttons : List (Button Msg.Msg)
buttons =
    [ Button.fromLabel "Ok" |> Button.cmd Msg.NoOp Button.primary
    , Button.fromLabel "Ok" |> Button.cmd Msg.NoOp Button.primary
    , Button.fromLabel "Ok" |> Button.cmd Msg.NoOp Button.primary
    ]


code : String
code =
    prettifyElmCode """
dialogV2 "Dialog title" Icon.warning
    |> Dialog.withBody ("Dialog body text" |> Text.body2 |> Text.renderElement cfg)
    |> Dialog.withButtons
        (Button.fromLabel "Ok" |> Button.cmd NoOp Button.primary)
"""
