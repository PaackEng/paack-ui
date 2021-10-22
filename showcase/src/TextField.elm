module TextField exposing (stories)

import Element exposing (Element)
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
        , errorTextFieldStory cfg
        , staticStory cfg
        , unitedStory cfg
        ]


defaultTextFieldStory : RenderConfig -> ExplorerStory
defaultTextFieldStory cfg =
    story
        ( "Default"
        , defaultTextFieldView cfg
        , pluginOptions defaultTextFieldCode
        )


defaultTextFieldView : RenderConfig -> Element RootMsg.Msg
defaultTextFieldView cfg =
    TextField.singlelineText (always RootMsg.NoOp)
        "My cool default"
        "Value"
        |> TextField.withPlaceholder "Enter your info here"
        |> TextField.setLabelVisible True
        |> TextField.renderElement cfg


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


errorTextFieldStory : RenderConfig -> ExplorerStory
errorTextFieldStory cfg =
    story
        ( "Input Error"
        , errorTextFieldView cfg
        , pluginOptions errorTextFieldCode
        )


errorTextFieldView : RenderConfig -> Element RootMsg.Msg
errorTextFieldView cfg =
    TextField.singlelineText (always RootMsg.NoOp)
        "My input with error"
        "Value"
        |> TextField.withPlaceholder "Enter your info here"
        |> TextField.withError "This field is required"
        |> TextField.setLabelVisible True
        |> TextField.renderElement cfg


errorTextFieldCode : String
errorTextFieldCode =
    """
TextField.singlelineText (always RootMsg.NoOp)
    "My input with error"
    "Value"
    |> TextField.withPlaceholder "Enter your info here"
    |> TextField.withError "This field is required"
    |> TextField.setLabelVisible True
    |> TextField.renderElement cfg

"""


usernameTextFieldStory : RenderConfig -> ExplorerStory
usernameTextFieldStory cfg =
    story
        ( "Username"
        , usernameTextFieldView cfg
        , pluginOptions usernameTextFieldCode
        )


usernameTextFieldView : RenderConfig -> Element RootMsg.Msg
usernameTextFieldView cfg =
    TextField.username (always RootMsg.NoOp)
        "Enter your username"
        "Value"
        |> TextField.setLabelVisible True
        |> TextField.renderElement cfg


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
        , passwordTextFieldView cfg
        , pluginOptions passwordTextFieldCode
        )


passwordTextFieldView : RenderConfig -> Element RootMsg.Msg
passwordTextFieldView cfg =
    TextField.currentPassword (always RootMsg.NoOp)
        "Enter your password"
        "Value"
        |> TextField.setLabelVisible True
        |> TextField.renderElement cfg


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
        , fullWidthView cfg
        , pluginOptions fullWidthCode
        )


fullWidthView : RenderConfig -> Element RootMsg.Msg
fullWidthView cfg =
    TextField.singlelineText (always RootMsg.NoOp)
        "My TextField"
        "Some big text"
        |> TextField.setLabelVisible True
        |> TextField.withWidth TextField.widthFull
        |> TextField.renderElement cfg


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


staticStory : RenderConfig -> ExplorerStory
staticStory cfg =
    story
        ( "Static Read-Only"
        , staticView cfg
        , pluginOptions staticCode
        )


staticView : RenderConfig -> Element RootMsg.Msg
staticView cfg =
    TextField.static
        "My ReadOnly TextField"
        "My ReadOnly TextField"
        |> TextField.withWidth TextField.widthFull
        |> TextField.renderElement cfg


staticCode : String
staticCode =
    """
-- Static
TextField.static
        "My TextField"
        "My TextField"
        |> TextField.setLabelVisible True
        |> TextField.withWidth TextField.widthFull
        |> TextField.renderElement cfg
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


unitedStory : RenderConfig -> ExplorerStory
unitedStory cfg =
    story
        ( "United"
        , unitedView cfg
        , pluginOptions fullWidthCode
        )


unitedView : RenderConfig -> Element RootMsg.Msg
unitedView cfg =
    Element.column [ Element.spacing 8 ]
        [ defaultTextFieldView cfg
        , usernameTextFieldView cfg
        , passwordTextFieldView cfg
        , fullWidthView cfg
        , errorTextFieldView cfg
        , staticView cfg
        ]
