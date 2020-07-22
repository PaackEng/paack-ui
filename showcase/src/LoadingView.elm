module LoadingView exposing (stories)

import Element exposing (..)
import PluginOptions exposing (PluginOptions, defaultWithMenu)
import UI.LoadingView
import UIExplorer exposing (storiesOf)
import Utils exposing (goToDocsCallToAction, prettifyElmCode, story, storyList)


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
        , pluginOptions "small"
        )


mediumStory =
    story
        ( "Medium"
        , viewBase
            UI.LoadingView.medium
        , pluginOptions "medium"
        )


largeStory =
    story
        ( "Large"
        , viewBase
            UI.LoadingView.large
        , pluginOptions "large"
        )


viewBase content =
    el [ width fill, height (px 300) ] content


pluginOptions : String -> PluginOptions
pluginOptions loadingViewType =
    { defaultWithMenu
        | code =
            prettifyElmCode ("LoadingView." ++ loadingViewType)
        , note = goToDocsCallToAction "LoadingView"
    }
