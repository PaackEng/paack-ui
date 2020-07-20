module LoadingView exposing (stories)

import Element exposing (..)
import UI.LoadingView
import UIExplorer exposing (storiesOf)
import Utils exposing (prettifyElmCode, story, storyList)


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
        , { note = ""
          , code = prettifyElmCode """
LoadingView.small
"""
          }
        )


mediumStory =
    story
        ( "Medium"
        , viewBase
            UI.LoadingView.medium
        , { note = ""
          , code = prettifyElmCode """
    LoadingView.medium
"""
          }
        )


largeStory =
    story
        ( "Large"
        , viewBase
            UI.LoadingView.large
        , { note = ""
          , code =
                prettifyElmCode
                    """
```elm
    LoadingView.large
```
"""
          }
        )


viewBase content =
    el [ width fill, height (px 300) ] content
