module TextField exposing (stories)

import Msg as RootMsg
import PluginOptions exposing (PluginOptions, defaultWithMenu)
import UI.RenderConfig exposing (RenderConfig)
import UI.TextField as TextField
import UIExplorer exposing (storiesOf)
import Utils
    exposing
        ( ExplorerStory
        , ExplorerUI
        , goToDocsCallToAction
        , prettifyElmCode
        , story
        )


stories : RenderConfig -> ExplorerUI
stories cfg =
    storiesOf
        "TextField"
        [ defaultTextFieldStory cfg
        , usernameTextFieldStory cfg
        , passwordTextFieldStory cfg
        , fullWidthStory cfg
        ]


defaultTextFieldStory : RenderConfig -> ExplorerStory
defaultTextFieldStory cfg =
    story
        ( "Default"
        , TextField.singlelineText (always RootMsg.NoOp)
            "My cool default"
            "Value"
            |> TextField.withPlaceholder "Enter your info here"
            |> TextField.setLabelVisible True
            |> TextField.renderElement cfg
        , pluginOptions defaultTextFieldCode
        )


defaultTextFieldCode : String
defaultTextFieldCode =
    """
-- Default
, TextField.singlelineText OnTextFieldChanged
        "My cool input"
        model.value
    |> TextField.withPlaceholder "Enter your info here"
    |> TextField.setLabelVisible True
    |> TextField.renderElement renderCfg
"""


usernameTextFieldStory : RenderConfig -> ExplorerStory
usernameTextFieldStory cfg =
    story
        ( "Username"
        , TextField.username (always RootMsg.NoOp)
            "Enter your username"
            "Value"
            |> TextField.setLabelVisible True
            |> TextField.renderElement cfg
        , pluginOptions usernameTextFieldCode
        )


usernameTextFieldCode : String
usernameTextFieldCode =
    """
-- Username
TextField.username OnTextFieldChanged
        "Enter your username"
        model.usernameValue
    |> TextField.setLabelVisible true
    |> TextField.renderElement renderCfg
"""


passwordTextFieldStory : RenderConfig -> ExplorerStory
passwordTextFieldStory cfg =
    story
        ( "Password"
        , TextField.currentPassword (always RootMsg.NoOp)
            "Enter your password"
            "Value"
            |> TextField.setLabelVisible True
            |> TextField.renderElement cfg
        , pluginOptions passwordTextFieldCode
        )


passwordTextFieldCode : String
passwordTextFieldCode =
    """
-- Password
TextField.currentPassword OnTextFieldChanged
        "Enter your password"
        mode.passwordValue
    |> TextField.setLabelVisible True
    |> TextField.renderElement renderCfg
"""


fullWidthStory : RenderConfig -> ExplorerStory
fullWidthStory cfg =
    story
        ( "Full Width"
        , TextField.singlelineText (always RootMsg.NoOp)
            "My TextField"
            "Some big text"
            |> TextField.setLabelVisible True
            |> TextField.withWidth TextField.widthFull
            |> TextField.renderElement cfg
        , pluginOptions fullWidthCode
        )


fullWidthCode : String
fullWidthCode =
    """
-- Full width
TextField.singlelineText OnTextFieldChanged
        "My TextField"
        model.someFieldValue
    |> TextField.setLabelVisible True
    |> TextField.withWidth TextField.widthFull
    |> TextField.renderElement renderCfg
"""


pluginOptions : String -> PluginOptions
pluginOptions code =
    { defaultWithMenu
        | code = prettifyElmCode (msgCode ++ code)
        , note = goToDocsCallToAction "TextField"
    }


msgCode : String
msgCode =
    """
type Msg
    = OnTextFieldChanged String
    | ...

"""
