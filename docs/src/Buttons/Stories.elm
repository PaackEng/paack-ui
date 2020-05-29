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
        , clearStory renderConfig
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


clearStory cfg =
    enabledStory cfg
        "Clear"
        Button.toneClear
        "Button.toneClear"


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
        collectionOfButtons cfg =
            row [ spacing 8 ]
                [ Button.bodyIcon (Icon.close "Close")
                    |> Button.button Msg.NoOp
                    |> Button.withTone Button.toneDanger
                    |> Button.toEl cfg
                , Button.bodyIcon (Icon.print "Print")
                    |> Button.button Msg.NoOp
                    |> Button.withTone Button.toneLight
                    |> Button.toEl cfg
                , Button.bodyIcon (Icon.toggle "Toggle")
                    |> Button.button Msg.NoOp
                    |> Button.withTone Button.toneLight
                    |> Button.toEl cfg
                ]
    in
    story
        ( "Contextual Sizes"
        , column [ width fill, spacing 12 ]
            [ text "Extra Large (Default) context, where the user attention is required!"
            , collectionOfButtons renderConfig
            , text "Large context, these don't compete with attention."
            , renderConfig
                |> RenderConfig.withContextualSize
                    RenderConfig.SizeLarge
                |> collectionOfButtons
            , text "Small context, unnecessary details."
            , renderConfig
                |> RenderConfig.withContextualSize
                    RenderConfig.SizeSmall
                |> collectionOfButtons
            ]
        , { note = """
```elm
{- Imagine the following part of your view: -}
collectionOfButtons cfg =
    row [ spacing 8 ]
        [ Button.bodyIcon (Icon.close "Close")
            |> Button.button YourMessage
            |> Button.withTone Button.toneDanger
            |> Button.toEl cfg
        , Button.bodyIcon (Icon.print "Print")
            |> Button.button YourMessage
            |> Button.withTone Button.toneLight
            |> Button.toEl cfg
        , Button.bodyIcon (Icon.toggle "Toggle")
            |> Button.button YourMessage
            |> Button.withTone Button.toneLight
            |> Button.toEl cfg
        ]

{- With context you can change size to gather more/less attention,
    WITHOUT THE FEAR of forgetting any (sub-)component -}

-- Extra Large (Default) context, where the user attention is required! 
defaultContext cfg =
    collectionOfButtons cfg

-- Large context, these don't compete with attention.
largeContext cfg =
    cfg
        |> RenderConfig.withContextualSize
            RenderConfig.SizeLarge
        |> collectionOfButtons

-- Small context, unnecessary details.
smallContext cfg =
    cfg
        |> RenderConfig.withContextualSize
            RenderConfig.SizeSmall
        |> collectionOfButtons
```
"""
          }
        )
