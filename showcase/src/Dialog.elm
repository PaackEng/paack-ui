module Dialog exposing (stories)

import Element
import Msg
import PluginOptions exposing (defaultWithoutMenu)
import UI.Button as Button exposing (Button)
import UI.Icon as Icon
import UI.Internal.Colors exposing (overlayBackground)
import UI.Internal.DialogV2 exposing (dialogViewV2)
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
            , Element.padding 20
            , Element.height <| Element.px 500
            , overlayBackground
            ]
            [ iconsSvgSprite
            , Dialog.dialog "Dialog title"
                (Icon.warning "Warning dialog")
                |> Dialog.withBody
                    ("Dialog body text" |> Text.body2 |> Text.renderElement cfg)
                |> Dialog.withButtons buttons
                |> dialogViewV2 cfg
            ]
        , { defaultWithoutMenu | code = code }
        )


buttons : List (Button Msg.Msg)
buttons =
    [ Button.fromLabel "Ok" |> Button.cmd Msg.NoOp Button.primary
    , Button.fromLabel "Cancel" |> Button.cmd Msg.NoOp Button.danger
    ]


code : String
code =
    prettifyElmCode """
dialog "Dialog title" Icon.warning closeMsg
    |> Dialog.withBody ("Dialog body text" |> Text.body2 |> Text.renderElement cfg)
    |> Dialog.withButtons
        [(Button.fromLabel "Ok" |> Button.cmd NoOp Button.primary)]
    |> Dialog.renderElement renderConfig
"""
