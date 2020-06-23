module LoadingView exposing (stories)

import Element exposing (..)
import UI.LoadingView
import UIExplorer exposing (storiesOf)
import Utils exposing (story, storyList)


stories =
    storiesOf
        "Loading"
        [ smallStory
        , mediumStory
        , largeStory
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


mediumStory =
    story
        ( "Medium"
        , viewBase
            UI.LoadingView.medium
        , { note =
                """
```elm
    LoadingView.medium
```
"""
          }
        )


largeStory =
    story
        ( "Large"
        , viewBase
            UI.LoadingView.large
        , { note =
                """
```elm
    LoadingView.large
```
"""
          }
        )


viewBase content =
    el [ width fill, height (px 300) ] content
