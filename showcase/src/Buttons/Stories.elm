module Buttons.Stories exposing (stories, update)

import Buttons.Model as Buttons
import Buttons.Msg as Buttons
import Element exposing (Element, fill)
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
        , unitedStory renderConfig
        ]


enabledStory : RenderConfig -> String -> ButtonStyle -> String -> ExplorerStory
enabledStory renderConfig label tone toneStr =
    storyList
        ( label
        , enabledView renderConfig tone
            |> (::) iconsSvgSprite
        , toneStr |> enabledCode |> pluginOptions
        )


enabledView : RenderConfig -> ButtonStyle -> List (Element Msg)
enabledView renderConfig tone =
    [ Button.fromLabel "Prompt"
        |> Button.cmd Msg.NoOp tone
        |> Button.renderElement renderConfig
    , Button.fromIcon (Icon.toggle "Toggle")
        |> Button.cmd Msg.NoOp tone
        |> Button.renderElement renderConfig
    ]


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
        , successView renderConfig |> (::) iconsSvgSprite
        , pluginOptions successCode
        )


successView : RenderConfig -> List (Element Msg)
successView renderConfig =
    [ Button.fromLabel "Prompt"
        |> Button.success
        |> Button.renderElement renderConfig
    , Button.fromIcon (Icon.toggle "Toggle")
        |> Button.cmd Msg.NoOp Button.primary
        |> Button.withSuccessIf True
        |> Button.renderElement renderConfig
    ]


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
        , linkView renderConfig
        , pluginOptions linkCode
        )


linkView : RenderConfig -> Element Msg
linkView renderConfig =
    Button.fromLabel "Go to Blank"
        |> Button.redirect (Link.link "about:blank") Button.hyperlink
        |> Button.renderElement renderConfig


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
        , fullWidthView renderConfig
        , pluginOptions fullWidthCode
        )


fullWidthView : RenderConfig -> Element Msg
fullWidthView renderConfig =
    Button.fromLabel "Super Long Prompt"
        |> Button.cmd Msg.NoOp Button.primary
        |> Button.withWidth Button.widthFull
        |> Button.renderElement renderConfig


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
        , toggleButtonView renderConfig
            buttonsStories.demoSwitch
        , Element.text "Click this Button!"
        , if buttonsStories.demoSwitch then
            Element.text "Something is active."

          else
            Element.text "Something is disabled."
        ]


toggleButtonView : RenderConfig -> Bool -> Element Msg
toggleButtonView renderConfig state =
    Button.toggle "Toggle what's there"
        (Msg.ButtonsStoriesMsg << Buttons.SetDemoSwitch)
        state
        |> Button.renderElement renderConfig


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


unitedStory : RenderConfig -> ExplorerStory
unitedStory renderConfig =
    story
        ( "United"
        , unitedView renderConfig
        , defaultWithMenu
        )


unitedView : RenderConfig -> Element Msg
unitedView renderConfig =
    Element.column [ Element.spacing 8, Element.width fill ]
        [ iconsSvgSprite
        , unitedItem <| enabledView renderConfig Button.primary
        , unitedItem <| enabledView renderConfig Button.danger
        , unitedItem <| enabledView renderConfig Button.light
        , unitedItem <| enabledView renderConfig Button.clear
        , unitedItem <| successView renderConfig
        , unitedItem <| [ linkView renderConfig ]
        , unitedItem <| [ fullWidthView renderConfig ]
        , unitedItem <| [ toggleButtonView renderConfig False, toggleButtonView renderConfig True ]
        ]


unitedItem : List (Element msg) -> Element msg
unitedItem content =
    Element.row [ Element.spacing 8 ]
        content
