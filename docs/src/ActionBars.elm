module ActionBars exposing (stories)

import Element exposing (..)
import Element.Background as Background
import Msg as Msg
import UI.ActionBar as ActionBar exposing (ActionBar)
import UI.Button as Button
import UI.Icons as Icons
import UI.Theme as Theme
import UIExplorer exposing (storiesOf)
import Utils exposing (story, storyList)


stories =
    storiesOf
        "ActionBars"
        [ defaultStory
        , withExtraButtonsStory
        , withCloseButtonStory
        , allFeaturesStory
        ]


defaultStory =
    story
        ( "Default"
        , viewBase
            (ActionBar.actionBar
                |> ActionBar.withTitle "Menu title"
                |> ActionBar.withSubtitle "Cool menu subtitle"
            )
        , { note =
                """
```elm
ActionBar.actionBar
    |> ActionBar.withTitle "Menu title"
    |> ActionBar.withSubtitle "Cool menu subtitle"
    |> ActionBar.toEl
```
"""
          }
        )


withExtraButtonsStory =
    story
        ( "Extra Buttons"
        , viewBase
            (ActionBar.actionBar
                |> ActionBar.withTitle "Menu title"
                |> ActionBar.withSubtitle "Cool menu subtitle"
                |> ActionBar.withButtons
                    [ Button.button Msg.NoOp
                        |> Button.withPrimaryColor
                        |> Button.withIcon (Icons.plus "Add item")
                    , Button.button Msg.NoOp
                        |> Button.withPrimaryColor
                        |> Button.withText "Make something great"
                    ]
            )
        , { note =
                """
```elm
ActionBar.actionBar
    |> ActionBar.withTitle "Menu title"
    |> ActionBar.withSubtitle "Cool menu subtitle"
    |> ActionBar.withButtons
        [ Button.button Msg.NoOp
            |> Button.withPrimaryColor
            |> Button.withIcon (Icons.plus "Add item")
        , Button.button Msg.NoOp
            |> Button.withPrimaryColor
            |> Button.withText "Make something great"
        ]
    |> ActionBar.toEl
```
"""
          }
        )


withCloseButtonStory =
    story
        ( "Close Button"
        , viewBase
            (ActionBar.actionBar
                |> ActionBar.withTitle "Menu title"
                |> ActionBar.withSubtitle "Cool menu subtitle"
                |> ActionBar.withCloseButton Msg.NoOp
            )
        , { note =
                """
```elm
ActionBar.actionBar
    |> ActionBar.withTitle "Menu title"
    |> ActionBar.withSubtitle "Cool menu subtitle"
    |> ActionBar.withCloseButton Msg.NoOp
    |> ActionBar.toEl
```
"""
          }
        )


allFeaturesStory =
    story
        ( "All Features"
        , viewBase
            (ActionBar.actionBar
                |> ActionBar.withTitle "Menu title"
                |> ActionBar.withSubtitle "Cool menu subtitle"
                |> ActionBar.withButtons
                    [ Button.button Msg.NoOp
                        |> Button.withPrimaryColor
                        |> Button.withIcon (Icons.plus "Add item")
                    , Button.button Msg.NoOp
                        |> Button.withPrimaryColor
                        |> Button.withText "Make something great"
                    ]
                |> ActionBar.withCloseButton Msg.NoOp
            )
        , { note =
                """
```elm
ActionBar.actionBar
    |> ActionBar.withTitle "Menu title"
    |> ActionBar.withSubtitle "Cool menu subtitle"
    |> ActionBar.withButtons
        [ Button.button Msg.NoOp
            |> Button.withPrimaryColor
            |> Button.withText (Icons.plus "Add item")
        , Button.button Msg.NoOp
            |> Button.withPrimaryColor
            |> Button.withText "Make something great"
        ]
    |> ActionBar.withOnCloseMessage Msg.NoOp
    |> ActionBar.toEl
```
"""
          }
        )


viewBase content =
    el [ width fill, height (px 300), Background.color Theme.black ] (ActionBar.toEl content)
