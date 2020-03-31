module PopUps exposing (stories)

import Element exposing (..)
import Element.Background as Background
import Msg
import UI.PopUp as PopUp
import UI.Theme as Theme
import UIExplorer exposing (storiesOf)
import Utils exposing (story)


stories =
    storiesOf
        "PopUps"
        [ collapsedStory
        , expandedStory
        , closableStory
        ]


collapsedStory =
    story
        ( "Collapsable PopUp (Collapsed)"
        , viewBase
            (PopUp.collapsable (always Msg.NoOp)
                |> PopUp.withTitle "PopUp Title"
                |> PopUp.withContent (el [ padding 20, centerX ] <| text "PopUp Content")
            )
        , { note = """
```elm
type Msg = TogglePopUp Bool | ...

PopUp.collapsable (TogglePopUp)
    |> PopUp.withTitle "PopUp Title"
    |> PopUp.withContent (el [ padding 20, centerX ] <| text "PopUp Content")
    |> PopUp.toEl
```
"""
          }
        )


expandedStory =
    story
        ( "Collapsable PopUp (Expanded)"
        , viewBase
            (PopUp.collapsable (always Msg.NoOp)
                |> PopUp.withTitle "PopUp Title"
                |> PopUp.withContent (el [ padding 20, centerX ] <| text "PopUp Content")
                |> PopUp.withCollapsed False
            )
        , { note = """
```elm
type Msg = TogglePopUp Bool | ...

PopUp.collapsable (TogglePopUp)
    |> PopUp.withTitle "PopUp Title"
    |> PopUp.withContent (el [ padding 20, centerX ] <| text "PopUp Content")
    |> PopUp.withCollapsed False
    |> PopUp.toEl
```
"""
          }
        )


closableStory =
    story
        ( "Closable PopUp"
        , viewBase
            (PopUp.closable Msg.NoOp
                |> PopUp.withTitle "PopUp Title"
                |> PopUp.withContent (el [ padding 20, centerX ] <| text "PopUp Content")
                |> PopUp.withCollapsed False
            )
        , { note = """
```elm
type Msg = ClosePopUp | ...

PopUp.closable ClosePopUp
    |> PopUp.withTitle "PopUp Title"
    |> PopUp.withContent (el [ padding 20, centerX ] <| text "PopUp Content")
    |> PopUp.withCollapsed False
    |> PopUp.toEl
```
"""
          }
        )


viewBase content =
    el [ width fill, height (px 300), Background.color Theme.black ] <|
        el [ alignBottom, alignRight ] (PopUp.toEl content)
