module Buttons.Stories exposing (stories, update)

import Buttons.Model as Buttons
import Buttons.Msg as Buttons
import Element exposing (column, fill, row, spacing, text, width)
import Msg as Msg
import Return as R exposing (Return)
import UI.Button as Button
import UI.Icon as Icon
import UI.Link as Link
import UI.RenderConfig as RenderConfig
import UIExplorer exposing (storiesOf)
import Utils exposing (iconsSvgSprite, story, storyList, storyWithModel)


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
        , clearStory renderConfig
        , linkStory renderConfig
        , fullWidthStory renderConfig
        , toggleStory renderConfig
        ]


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
        , { note = """
```elm
-- Text

Button.fromLabel "Some Text"
    |> Button.cmd YourMessage """ ++ toneStr ++ """
    |> Button.renderElement renderConfig


-- Icon
Button.fromIcon Icon.someIcon
    |> Button.cmd YourMessage """ ++ toneStr ++ """
    |> Button.renderElement renderConfig
```
"""
          }
        )


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
        , { note = """
```elm
-- Text

Button.fromLabel "Some Text"
    |> Button.disabled
    |> Button.renderElement renderConfig


-- Icon
Button.fromIcon Icon.someIcon
    |> Button.cmd YourMessage
    |> Button.withDisabledIf True
    |> Button.renderElement renderConfig
```
"""
          }
        )


primaryStory cfg =
    enabledStory cfg
        "Primary"
        Button.primary
        "Button.primary"


dangerStory cfg =
    enabledStory cfg
        "Danger"
        Button.danger
        "Button.danger"


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
        , { note = """
```elm
-- Text

Button.fromLabel "Some Text"
    |> Button.success
    |> Button.renderElement renderConfig


-- Icon
Button.fromIcon Icon.someIcon
    |> Button.cmd YourMessage
    |> Button.withSuccessIf True
    |> Button.renderElement renderConfig
```
"""
          }
        )


lightStory cfg =
    enabledStory cfg
        "Light"
        Button.light
        "Button.light"


clearStory cfg =
    enabledStory cfg
        "Clear"
        Button.clear
        "Button.clear"


linkStory renderConfig =
    story
        ( "Link"
        , Button.fromLabel "Go to Blank"
            |> Button.redirect (Link.link "about:blank") Button.hyperlink
            |> Button.renderElement renderConfig
        , { note = """
```elm
Button.fromLabel "Go to Blank"
    |> Button.redirect (Link.link "about:blank") Button.hyperlink
    |> Button.renderElement renderConfig
```
""" }
        )


fullWidthStory renderConfig =
    story
        ( "Full Width"
        , Button.fromLabel "Super Long Prompt"
            |> Button.cmd Msg.NoOp Button.primary
            |> Button.withWidth Button.full
            |> Button.renderElement renderConfig
        , { note = """
```elm
Button.fromLabel "Some Text"
    |> Button.cmd YourMessage Button.primary
    |> Button.withWidth Button.full
    |> Button.renderElement renderConfig
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
                    [ iconsSvgSprite
                    , Button.toggle "Toggle what's there" msg buttonsStories.demoSwitch
                        |> Button.renderElement renderConfig
                    , if buttonsStories.demoSwitch then
                        Element.text "Click this Button!"

                      else
                        Element.text "Why did you do it?"
                    ]
    in
    storyWithModel
        ( "Toggle", body, { note = """
```elm
Button.toggle "Some Hint" YourMessage TrueOrFalse
    |> Button.renderElement renderConfig
```
""" } )
