module Buttons.Stories exposing (stories, update)

import Buttons.Model as Buttons
import Buttons.Msg as Buttons
import Element exposing (Element, column, spacing, text)
import Model exposing (Model)
import Msg as Msg exposing (Msg)
import PluginOptions exposing (PluginOptions, defaultWithMenu)
import Return exposing (Return)
import UI.Button as Button exposing (ButtonStyle)
import UI.Icon as Icon
import UI.Link as Link
import UI.RenderConfig exposing (RenderConfig)
import UIExplorer exposing (storiesOf)
import Utils
    exposing
        ( ExplorerStory
        , ExplorerUI
        , goToDocsCallToAction
        , iconsSvgSprite
        , prettifyElmCode
        , story
        , storyList
        , storyWithModel
        )


update : Buttons.Msg -> Buttons.Model -> Return Buttons.Msg Buttons.Model
update msg model =
    case msg of
        Buttons.SetDemoSwitch newValue ->
            ( { model | demoSwitch = newValue }, Cmd.none )


stories : RenderConfig -> ExplorerUI
stories renderConfig =
    storiesOf
        "Buttons"
        [ primaryStory renderConfig
        , disabledButtonStory renderConfig
        , successStory renderConfig
        , dangerStory renderConfig
        , lightStory renderConfig
        , clearStory renderConfig
        , linkStory renderConfig
        , fullWidthStory renderConfig
        , toggleStory renderConfig
        ]


enabledStory : RenderConfig -> String -> ButtonStyle -> String -> ExplorerStory
enabledStory renderConfig label tone toneStr =
    storyList
        ( label
        , [ iconsSvgSprite
          , Button.fromLabel "Prompt"
                |> Button.cmd Msg.NoOp tone
                |> Button.renderElement renderConfig
          , Button.fromIcon (Icon.toggle "Toggle")
                |> Button.cmd Msg.NoOp tone
                |> Button.renderElement renderConfig
          ]
        , toneStr |> enabledCode |> pluginOptions
        )


enabledCode : String -> String
enabledCode toneStr =
    """
-- Text

Button.fromLabel "Some Text"
    |> Button.cmd YourMessage """
        ++ toneStr
        ++ """
    |> Button.renderElement renderConfig


-- Icon
Button.fromIcon Icon.someIcon
    |> Button.cmd YourMessage """
        ++ toneStr
        ++ """
    |> Button.renderElement renderConfig
"""


disabledButtonStory : RenderConfig -> ExplorerStory
disabledButtonStory renderConfig =
    storyList
        ( "Disabled"
        , [ iconsSvgSprite
          , Button.fromLabel "Prompt"
                |> Button.disabled
                |> Button.renderElement renderConfig
          , Button.fromIcon (Icon.toggle "Toggle")
                |> Button.cmd Msg.NoOp Button.primary
                |> Button.withDisabledIf True
                |> Button.renderElement renderConfig
          ]
        , pluginOptions disabledButtonCode
        )


disabledButtonCode : String
disabledButtonCode =
    """
-- Text

Button.fromLabel "Some Text"
    |> Button.disabled
    |> Button.renderElement renderConfig


-- Icon
Button.fromIcon Icon.someIcon
    |> Button.cmd YourMessage
    |> Button.withDisabledIf True
    |> Button.renderElement renderConfig
"""


primaryStory : RenderConfig -> ExplorerStory
primaryStory cfg =
    enabledStory cfg
        "Primary"
        Button.primary
        "Button.primary"


dangerStory : RenderConfig -> ExplorerStory
dangerStory cfg =
    enabledStory cfg
        "Danger"
        Button.danger
        "Button.danger"


successStory : RenderConfig -> ExplorerStory
successStory renderConfig =
    storyList
        ( "Success"
        , [ iconsSvgSprite
          , Button.fromLabel "Prompt"
                |> Button.success
                |> Button.renderElement renderConfig
          , Button.fromIcon (Icon.toggle "Toggle")
                |> Button.cmd Msg.NoOp Button.primary
                |> Button.withSuccessIf True
                |> Button.renderElement renderConfig
          ]
        , pluginOptions successCode
        )


successCode : String
successCode =
    """
-- Text

Button.fromLabel "Some Text"
    |> Button.success
    |> Button.renderElement renderConfig


-- Icon
Button.fromIcon Icon.someIcon
    |> Button.cmd YourMessage
    |> Button.withSuccessIf True
    |> Button.renderElement renderConfig
"""


lightStory : RenderConfig -> ExplorerStory
lightStory cfg =
    enabledStory cfg
        "Light"
        Button.light
        "Button.light"


clearStory : RenderConfig -> ExplorerStory
clearStory cfg =
    enabledStory cfg
        "Clear"
        Button.clear
        "Button.clear"


linkStory : RenderConfig -> ExplorerStory
linkStory renderConfig =
    story
        ( "Link"
        , Button.fromLabel "Go to Blank"
            |> Button.redirect (Link.link "about:blank") Button.hyperlink
            |> Button.renderElement renderConfig
        , pluginOptions linkCode
        )


linkCode : String
linkCode =
    """
Button.fromLabel "Go to Blank"
    |> Button.redirect (Link.link "about:blank") Button.hyperlink
    |> Button.renderElement renderConfig
"""


fullWidthStory : RenderConfig -> ExplorerStory
fullWidthStory renderConfig =
    story
        ( "Full Width"
        , Button.fromLabel "Super Long Prompt"
            |> Button.cmd Msg.NoOp Button.primary
            |> Button.withWidth Button.widthFull
            |> Button.renderElement renderConfig
        , pluginOptions fullWidthCode
        )


fullWidthCode : String
fullWidthCode =
    """
Button.fromLabel "Some Text"
    |> Button.cmd YourMessage Button.primary
    |> Button.withWidth Button.widthFull
    |> Button.renderElement renderConfig
"""


toggleStory : RenderConfig -> ExplorerStory
toggleStory renderConfig =
    storyWithModel
        ( "Toggle"
        , toggleView renderConfig
        , pluginOptions toggleCode
        )


toggleView : RenderConfig -> Model -> Element Msg
toggleView renderConfig { buttonsStories } =
    Element.column [ Element.spacing 20 ]
        [ iconsSvgSprite
        , Button.toggle "Toggle what's there"
            (Msg.ButtonsStoriesMsg << Buttons.SetDemoSwitch)
            buttonsStories.demoSwitch
            |> Button.renderElement renderConfig
        , Element.text "Click this Button!"
        , if buttonsStories.demoSwitch then
            Element.text "Something is active."

          else
            Element.text "Something is disabled."
        ]


toggleCode : String
toggleCode =
    """
Button.toggle "Some Hint" YourMessage TrueOrFalse
    |> Button.renderElement renderConfig
"""


pluginOptions : String -> PluginOptions
pluginOptions code =
    { defaultWithMenu
        | code = prettifyElmCode code
        , note = goToDocsCallToAction "Button"
    }
