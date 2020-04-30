module Buttons exposing (stories)

import Element
import Msg as Msg
import UI.Button as Btn
import UI.Icons as Ico
import UIExplorer exposing (storiesOf)
import Utils exposing (story, storyList)


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
        , outlineStory renderConfig
        ]


enabledStory renderConfig label tone toneStr =
    storyList
        ( label
        , [ Btn.bodyText "Prompt"
                |> Btn.button Msg.NoOp
                |> Btn.withTone tone
                |> Btn.toEl renderConfig
          , Btn.bodyIcon Ico.toggle
                |> Btn.button Msg.NoOp
                |> Btn.withTone tone
                |> Btn.toEl renderConfig
          ]
        , { note = """
```elm
-- Text

Button.bodyText "Some Text"
    |> Button.button YourMessage
    |> Button.withTone """ ++ toneStr ++ """
    |> Button.toEl renderConfig


-- Icon
Btn.bodyIcon Ico.someIcon
    |> Btn.button YourMessage
    |> Btn.withTone """ ++ toneStr ++ """
    |> Btn.toEl renderConfig
```
"""
          }
        )


disabledButtonStory renderConfig =
    storyList
        ( "Disabled"
        , [ Btn.bodyText "Prompt"
                |> Btn.button Msg.NoOp
                |> Btn.withMode Btn.modeDisabled
                |> Btn.toEl renderConfig
          , Btn.bodyIcon Ico.toggle
                |> Btn.button Msg.NoOp
                |> Btn.withMode Btn.modeDisabled
                |> Btn.toEl renderConfig
          ]
        , { note = """
```elm
-- Text

Button.bodyText "Some Text"
    |> Button.button YourMessage
    |> Btn.withMode Btn.modeDisabled
    |> Button.toEl renderConfig


-- Icon
Btn.bodyIcon Ico.someIcon
    |> Btn.button YourMessage
    |> Btn.withMode Btn.modeDisabled
    |> Btn.toEl renderConfig
```
"""
          }
        )


primaryStory cfg =
    enabledStory cfg
        "Primary"
        Btn.tonePrimary
        "Btn.tonePrimary"


dangerStory cfg =
    enabledStory cfg
        "Danger"
        Btn.toneDanger
        "Btn.toneDanger"


successStory cfg =
    enabledStory cfg
        "Success"
        Btn.toneSuccess
        "Btn.toneSuccess"


lightStory cfg =
    enabledStory cfg
        "Light"
        Btn.toneLight
        "Btn.toneLight"


linkStory renderConfig =
    story
        ( "Link"
        , Btn.bodyText "Go to Blank"
            |> Btn.link "about:blank"
            |> Btn.toEl renderConfig
        , { note = """
```elm
-- Text
Btn.bodyText "Go to Blank"
    |> Btn.link "about:blank"
    |> Btn.toEl renderConfig
```
""" }
        )


fullWidthStory renderConfig =
    story
        ( "Full Width"
        , Btn.bodyText "Super Long Prompt"
            |> Btn.button Msg.NoOp
            |> Btn.withWidth Btn.widthFull
            |> Btn.toEl renderConfig
        , { note = """
```elm
-- Text

Btn.bodyText "Some Text"
    |> Btn.button Your Message
    |> Btn.withWidth Btn.widthFull
    |> Btn.toEl renderConfig
```
""" }
        )


outlineStory renderConfig =
    storyList
        ( "Outline"
        , [ Btn.bodyText "Prompt"
                |> Btn.button Msg.NoOp
                |> Btn.withStyle Btn.styleOutlined
                |> Btn.withTone Btn.toneDanger
                |> Btn.toEl renderConfig
          , Btn.bodyIcon Ico.toggle
                |> Btn.button Msg.NoOp
                |> Btn.withStyle Btn.styleOutlined
                |> Btn.withTone Btn.toneDanger
                |> Btn.toEl renderConfig
          ]
        , { note = """
```elm
-- Text

Button.bodyText "Some Text"
    |> Button.button YourMessage
    |> Btn.withStyle Btn.styleOutlined
    |> Btn.withTone Btn.toneDanger
    |> Button.toEl renderConfig


-- Icon
Btn.bodyIcon Ico.someIcon
    |> Btn.button YourMessage
    |> Btn.withStyle Btn.styleOutlined
    |> Btn.withTone Btn.toneDanger
    |> Btn.toEl renderConfig
```
""" }
        )
