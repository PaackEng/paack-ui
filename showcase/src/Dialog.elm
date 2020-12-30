module Dialog exposing (stories)

import Element
import Msg
import PluginOptions exposing (defaultWithMenu)
import UI.Button as Button exposing (Button)
import UI.Icon as Icon
import UI.Internal.DialogV2 exposing (dialogViewV2)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.V2.Dialog as Dialog
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, iconsSvgSprite, prettifyElmCode, story)


stories : RenderConfig -> ExplorerUI
stories cfg =
    storiesOf
        "Dialog"
        [ dialogDesktop cfg
        , dialogMobile
        ]


dialogDesktop : RenderConfig -> ExplorerStory
dialogDesktop cfg =
    story
        ( "Dialog Desktop"
        , Element.column
            [ Element.centerX
            , Element.centerY
            , Element.padding 20
            , Element.height <| Element.px 500
            , Element.width Element.fill
            ]
            [ iconsSvgSprite
            , Dialog.dialog demoText.title
                (Icon.warning demoText.icon)
                |> Dialog.withBody
                    (demoText.body |> Text.body2 |> Text.renderElement cfg)
                |> Dialog.withButtons buttons
                |> dialogViewV2 cfg
            ]
        , { defaultWithMenu | code = code }
        )


dialogMobile : ExplorerStory
dialogMobile =
    story
        ( "Dialog Mobile"
        , Element.column
            [ Element.height <| Element.px mobileResolution.height
            , Element.width <| Element.px mobileResolution.width
            , Element.centerX
            ]
            [ iconsSvgSprite
            , Dialog.dialog demoText.title
                (Icon.warning demoText.icon)
                |> Dialog.withBody
                    (demoText.body
                        |> Text.body2
                        |> Text.renderElement
                            mobileCfg
                    )
                |> Dialog.withButtons buttons
                |> dialogViewV2 mobileCfg
            ]
        , { defaultWithMenu | code = code }
        )


demoText : { title : String, body : String, icon : String }
demoText =
    { title = "Dialog title"
    , body = "There is no one who loves pain itself, who seeks after it and wants to have it, simply because it is pain..."
    , icon = "Warning dialog"
    }


mobileResolution : { height : Int, width : Int }
mobileResolution =
    { width = 375, height = 667 }


mobileCfg : RenderConfig
mobileCfg =
    RenderConfig.init
        mobileResolution
        RenderConfig.localeEnglish


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
