module Buttons.Stories exposing (stories, update)

import Buttons.Model as Buttons
import Buttons.Msg as Buttons
import Element
import Msg as Msg
import Return as R exposing (Return)
import UI.Button as Button
import UI.Icon as Icon
import UI.Link as Link
import UI.RenderConfig as RenderConfig
import UIExplorer exposing (storiesOf)
import Utils exposing (story, storyList, storyWithModel)


update : Buttons.Msg -> Buttons.Model -> Return Buttons.Msg Buttons.Model
update msg model =
    case msg of
        Buttons.SetDemoSwitch newValue ->
            ( { model | demoSwitch = newValue }, Cmd.none )


stories renderConfig =
    storiesOf
        "Buttons"
        [ primaryStory renderConfig
        , disabledButtonStory renderConfig
        , successStory renderConfig
        , dangerStory renderConfig
        , lightStory renderConfig
        , linkStory renderConfig
        , fullWidthStory renderConfig
        , toggleStory renderConfig
        , contextualSizeStory renderConfig
        ]


enabledStory renderConfig label tone toneStr =
    storyList
        ( label
        , [ Button.bodyText "Prompt"
                |> Button.button Msg.NoOp
                |> Button.withTone tone
                |> Button.toEl renderConfig
          , Button.bodyIcon (Icon.toggle "Toggle")
                |> Button.button Msg.NoOp
                |> Button.withTone tone
                |> Button.toEl renderConfig
          ]
        , { note = """
```elm
-- Text

Button.bodyText "Some Text"
    |> Button.button YourMessage
    |> Button.withTone """ ++ toneStr ++ """
    |> Button.toEl renderConfig


-- Icon
Button.bodyIcon Icon.someIcon
    |> Button.button YourMessage
    |> Button.withTone """ ++ toneStr ++ """
    |> Button.toEl renderConfig
```
"""
          }
        )


disabledButtonStory renderConfig =
    storyList
        ( "Disabled"
        , [ Button.bodyText "Prompt"
                |> Button.button Msg.NoOp
                |> Button.withMode Button.modeDisabled
                |> Button.toEl renderConfig
          , Button.bodyIcon (Icon.toggle "Toggle")
                |> Button.button Msg.NoOp
                |> Button.withMode Button.modeDisabled
                |> Button.toEl renderConfig
          ]
        , { note = """
```elm
-- Text

Button.bodyText "Some Text"
    |> Button.button YourMessage
    |> Button.withMode Button.modeDisabled
    |> Button.toEl renderConfig


-- Icon
Button.bodyIcon Icon.someIcon
    |> Button.button YourMessage
    |> Button.withMode Button.modeDisabled
    |> Button.toEl renderConfig
```
"""
          }
        )


primaryStory cfg =
    enabledStory cfg
        "Primary"
        Button.tonePrimary
        "Button.tonePrimary"


dangerStory cfg =
    enabledStory cfg
        "Danger"
        Button.toneDanger
        "Button.toneDanger"


successStory cfg =
    enabledStory cfg
        "Success"
        Button.toneSuccess
        "Button.toneSuccess"


lightStory cfg =
    enabledStory cfg
        "Light"
        Button.toneLight
        "Button.toneLight"


linkStory renderConfig =
    story
        ( "Link"
        , Button.bodyText "Go to Blank"
            |> Button.link (Link.link "about:blank")
            |> Button.toEl renderConfig
        , { note = """
```elm
Button.bodyText "Go to Blank"
    |> Button.link (Link.link "about:blank")
    |> Button.toEl renderConfig
```
""" }
        )


fullWidthStory renderConfig =
    story
        ( "Full Width"
        , Button.bodyText "Super Long Prompt"
            |> Button.button Msg.NoOp
            |> Button.withWidth Button.widthFull
            |> Button.toEl renderConfig
        , { note = """
```elm
Button.bodyText "Some Text"
    |> Button.button YourMessage
    |> Button.withWidth Button.widthFull
    |> Button.toEl renderConfig
```
""" }
        )


toggleStory renderConfig =
    let
        msg =
            Msg.ButtonsStoriesMsg << Buttons.SetDemoSwitch

        body =
            \{ buttonsStories } ->
                Element.column [ Element.spacing 20 ]
                    [ Button.bodyIcon (Icon.toggle "Toggle what's there")
                        |> Button.toggle msg buttonsStories.demoSwitch
                        |> Button.toEl renderConfig
                    , if buttonsStories.demoSwitch then
                        Element.text "Click this Button!"

                      else
                        Element.text "Why did you do it?"
                    ]
    in
    storyWithModel
        ( "Toggle", body, { note = """
```elm
Button.bodyIcon (Icon.toggle "Some Hint")
    |> Button.toggle YourMessage TrueOrFalse
    |> Button.toEl renderConfig
```
""" } )


contextualSizeStory renderConfig =
    let
        largeRenderConfig =
            RenderConfig.withContextualSize RenderConfig.SizeLarge renderConfig

        smallRenderConfig =
            RenderConfig.withContextualSize RenderConfig.SizeSmall renderConfig
    in
    storyList
        ( "Contextual Sizes"
        , [ Button.bodyText "Prompt"
                |> Button.button Msg.NoOp
                |> Button.withTone Button.toneSuccess
                |> Button.toEl renderConfig
          , Button.bodyIcon (Icon.toggle "Toggle")
                |> Button.button Msg.NoOp
                |> Button.withTone Button.toneDanger
                |> Button.toEl largeRenderConfig
          , Button.bodyIcon (Icon.toggle "Toggle")
                |> Button.button Msg.NoOp
                |> Button.withTone Button.toneLight
                |> Button.toEl smallRenderConfig
          ]
        , { note = """
```elm
-- Default (ExtraLarge)
Button.bodyText "Some Text"
    |> Button.button YourMessage
    |> Button.withTone Button.toneSuccess
    |> Button.toEl renderConfig


-- Large
Button.bodyIcon Icon.someIcon
    |> Button.button YourMessage
    |> Button.withTone Button.toneDanger
    |> Button.toEl (RenderConfig.withContextualSize RenderConfig.SizeLarge renderConfig)

-- Small
Button.bodyIcon Icon.someIcon
    |> Button.button YourMessage
    |> Button.withTone Button.toneLight
    |> Button.toEl (RenderConfig.withContextualSize RenderConfig.SizeSmall renderConfig)
```
"""
          }
        )
