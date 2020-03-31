module Buttons exposing (stories)

import Element as El
import Element.Background as Background
import Msg as Msg
import UI.Button as B
import UI.Colors as Colors
import UI.Icons as Icons
import UIExplorer exposing (storiesOf)
import Utils exposing (story, storyList)


baseButton : B.Button Msg.Msg
baseButton =
    B.button Msg.NoOp
        |> B.withText "Click here"


stories =
    storiesOf
        "Buttons"
        [ primaryStory
        , successStory
        , warningStory
        , dangerStory
        , disabledButtonStory
        , toggleStory
        , fullWidthStory
        , linkStory
        ]


disabledButtonStory =
    let
        stateText isEnabled =
            if isEnabled then
                "Disabled"

            else
                "Enabled"

        button isDisabled =
            El.column [ El.spacing 10 ]
                [ B.button Msg.NoOp
                    |> B.withPrimaryColor
                    |> B.withDisabledMode isDisabled
                    |> B.withIcon (Icons.map "Show Map")
                    |> B.toEl
                , El.el [ El.centerX ] <| El.text (stateText isDisabled)
                ]
    in
    storyList
        ( "Disabled"
        , [ button True
          , button False
          ]
        , { note = """
```elm
-- Disabled

B.button YourMessage 
    |> B.withPrimaryColor
    |> B.withDisabledMode True
    |> B.withIcon (Icons.map "Show Map")
    |> B.toEl

-- Enabled

B.button YourMessage 
    |> B.withPrimaryColor
    |> B.withDisabledMode False
    |> B.withIcon (Icons.map "Show Map")
    |> B.toEl
```
"""
          }
        )


fullWidthStory =
    story
        ( "Full Width"
        , El.el
            [ Background.color Colors.black
            , El.width (El.px 400)
            , El.height (El.px 400)
            ]
            (baseButton
                |> B.withPrimaryColor
                |> B.withFullWidth
                |> B.toEl
            )
        , { note = """
```elm
B.button YourMessage
    |> B.withText "Click here"
    |> B.withPrimaryColor
    |> B.withFullWidth
    |> B.toEl
```
"""
          }
        )


toggleStory =
    let
        stateText isEnabled =
            if isEnabled then
                "On"

            else
                "Off"

        button isEnabled =
            El.column [ El.spacing 10 ]
                [ B.toggle (always Msg.NoOp) isEnabled
                    |> B.withPrimaryColor
                    |> B.withIcon (Icons.map "Show Map")
                    |> B.toEl
                , El.el [ El.centerX ] <| El.text (stateText isEnabled)
                ]
    in
    storyList
        ( "Toggle"
        , [ button True
          , button False
          ]
        , { note = """
```elm
B.toggle YourOwnMessage True
    |> B.withText "Click here"
    |> B.withDangerColor
    |> B.toEl

B.toggle YourOwnMessage False
    |> B.withText "Click here"
    |> B.withDangerColor
    |> B.toEl
```
"""
          }
        )


linkStory =
    story
        ( "Link"
        , B.link "http://www.google.com"
            |> B.withPrimaryColor
            |> B.withText "Go to google"
            |> B.toEl
        , { note = """
```elm
B.link "http://www.google.com"
    |> B.withPrimaryColor
    |> B.withText "Go to google"
    |> B.toEl
```
"""
          }
        )


primaryStory =
    storyList
        ( "Primary"
        , [ baseButton
                |> B.withPrimaryColor
                |> B.toEl
          , baseButton
                |> B.withPrimaryColor
                |> B.withIcon (Icons.map "Map")
                |> B.toEl
          ]
        , { note = """
```elm
-- Text

B.button YourMessage
    |> B.withText "Click here"
    |> B.withPrimaryColor
    |> B.toEl


-- Icon

B.button YourMessage
    |> B.withIcon (Icons.map "Map")
    |> B.withPrimaryColor
    |> B.toEl
```
"""
          }
        )


successStory =
    story
        ( "Success"
        , baseButton
            |> B.withSuccessColor
            |> B.toEl
        , { note = """
```elm
B.button YourMessage
    |> B.withText "Click here"
    |> B.withSuccessColor
    |> B.toEl
```
"""
          }
        )


warningStory =
    story
        ( "Warning"
        , baseButton
            |> B.withWarningColor
            |> B.toEl
        , { note = """
```elm
B.button YourMessage
    |> B.withText "Click here"
    |> B.withWarningColor
    |> B.toEl
```
"""
          }
        )


dangerStory =
    story
        ( "Danger"
        , baseButton
            |> B.withDangerColor
            |> B.toEl
        , { note = """
```elm
B.button YourMessage
    |> B.withText "Click here"
    |> B.withDangerColor
    |> B.toEl
```
"""
          }
        )
