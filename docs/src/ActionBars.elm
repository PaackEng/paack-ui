module ActionBars exposing (stories)

import Element exposing (..)
import Element.Background as Background
import Msg as Msg
import UI.ActionBar as ActionBar exposing (ActionBar)
import UI.Button as Button
import UI.Icon as Icons
import UIExplorer exposing (storiesOf)
import Utils exposing (story, storyList)


stories cfg =
    storiesOf
        "ActionBars"
        [ defaultStory cfg
        , withExtraButtonsStory cfg
        , withCloseButtonStory cfg
        , allFeaturesStory cfg
        ]


defaultStory cfg =
    story
        ( "Default"
        , viewBase cfg
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


withExtraButtonsStory cfg =
    story
        ( "Extra Buttons"
        , viewBase cfg
            (ActionBar.actionBar
                |> ActionBar.withTitle "Menu title"
                |> ActionBar.withSubtitle "Cool menu subtitle"
                |> ActionBar.withButtons
                    [ Button.bodyIcon (Icons.toggle "Toggle something")
                        |> Button.button Msg.NoOp
                        |> Button.withTone Button.tonePrimary
                    , Button.bodyText "Make something great"
                        |> Button.button Msg.NoOp
                        |> Button.withTone Button.tonePrimary
                    ]
            )
        , { note =
                """
```elm
ActionBar.actionBar
    |> ActionBar.withTitle "Menu title"
    |> ActionBar.withSubtitle "Cool menu subtitle"
    |> ActionBar.withButtons
        [ Button.bodyIcon (Icons.toggle "Toggle something")
            |> Button.button Msg.NoOp
            |> Button.withTone Button.tonePrimary
        , Button.bodyText "Make something great"
            |> Button.button Msg.NoOp
            |> Button.withTone Button.tonePrimary
        ]
    |> ActionBar.toEl
```
"""
          }
        )


withCloseButtonStory cfg =
    story
        ( "Close Button"
        , viewBase cfg
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


allFeaturesStory cfg =
    story
        ( "All Features"
        , viewBase cfg
            (ActionBar.actionBar
                |> ActionBar.withTitle "Menu title"
                |> ActionBar.withSubtitle "Cool menu subtitle"
                |> ActionBar.withButtons
                    [ Button.bodyIcon (Icons.toggle "Toggle something")
                        |> Button.button Msg.NoOp
                        |> Button.withTone Button.tonePrimary
                    , Button.bodyText "Make something great"
                        |> Button.button Msg.NoOp
                        |> Button.withTone Button.tonePrimary
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
        [ Button.bodyIcon (Icons.toggle "Toggle something")
            |> Button.button Msg.NoOp
            |> Button.withTone Button.tonePrimary
        , Button.bodyText "Make something great"
            |> Button.button Msg.NoOp
            |> Button.withTone Button.tonePrimary
        ]
    |> ActionBar.withOnCloseMessage Msg.NoOp
    |> ActionBar.toEl
```
"""
          }
        )


viewBase cfg content =
    content
        |> ActionBar.renderElement cfg
