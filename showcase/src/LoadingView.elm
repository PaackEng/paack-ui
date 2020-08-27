module LoadingView exposing (stories)

import Element exposing (Element, el, fill, height, px, width)
import PluginOptions exposing (PluginOptions, defaultWithMenu)
import UI.LoadingView as LoadingView
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, goToDocsCallToAction, prettifyElmCode, story)


stories : ExplorerUI
stories =
    storiesOf
        "Loading"
        [ smallStory
        , mediumStory
        , largeStory
        ]


smallStory : ExplorerStory
smallStory =
    story
        ( "Small"
        , viewBase
            LoadingView.small
        , pluginOptions "small"
        )


mediumStory : ExplorerStory
mediumStory =
    story
        ( "Medium"
        , viewBase
            LoadingView.medium
        , pluginOptions "medium"
        )


largeStory : ExplorerStory
largeStory =
    story
        ( "Large"
        , viewBase
            LoadingView.large
        , pluginOptions "large"
        )


viewBase : Element msg -> Element msg
viewBase content =
    el [ width fill, height (px 300) ] content


pluginOptions : String -> PluginOptions
pluginOptions loadingViewType =
    { defaultWithMenu
        | code = code loadingViewType
        , note = goToDocsCallToAction "LoadingView"
    }


code : String -> String
code loadingViewType =
    loadingViewType
        |> (++) "LoadingView."
        |> prettifyElmCode
