module LoadingView exposing (stories)

import Element exposing (..)
import UI.LoadingView
import UIExplorer exposing (storiesOf)
import Utils exposing (story, storyList)


stories =
    storiesOf
        "Loading"
        [ smallStory
        , defaultStory
        , bigStory
        ]


smallStory =
    story
        ( "Small"
        , viewBase
            UI.LoadingView.small
        , { note =
                """
```elm
    LoadingView.small
```
"""
          }
        )


defaultStory =
    story
        ( "Default"
        , viewBase
            UI.LoadingView.default
        , { note =
                """
```elm
    LoadingView.default
```
"""
          }
        )


bigStory =
    story
        ( "Big"
        , viewBase
            UI.LoadingView.big
        , { note =
                """
```elm
    LoadingView.big
```
"""
          }
        )


viewBase content =
    el [ width fill, height (px 300) ] content
