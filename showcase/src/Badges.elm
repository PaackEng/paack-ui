module Badges exposing (stories)

import PluginOptions exposing (defaultWithMenu)
import UI.Badge as Badge exposing (Badge)
import UI.RenderConfig exposing (RenderConfig)
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, goToDocsCallToAction, prettifyElmCode, storyList)


stories : RenderConfig -> ExplorerUI
stories cfg =
    storiesOf
        "Badges"
        [ oneBadge cfg Badge.grayLight Badge.grayDark "gray"
        , oneBadge cfg Badge.primaryLight Badge.primaryDark "primary"
        , oneBadge cfg Badge.warningLight Badge.warningDark "warning"
        , oneBadge cfg Badge.dangerLight Badge.dangerDark "danger"
        , oneBadge cfg Badge.successLight Badge.successDark "success"
        ]


oneBadge :
    RenderConfig
    -> (String -> Badge)
    -> (String -> Badge)
    -> String
    -> ExplorerStory
oneBadge cfg constructorLight constructorDark variation =
    storyList
        ( "Badge " ++ variation
        , List.map (Badge.renderElement cfg)
            [ constructorLight "123"
            , constructorDark "456"
            ]
        , { defaultWithMenu
            | code = code variation
            , note = goToDocsCallToAction "Badge"
          }
        )


code : String -> String
code variation =
    prettifyElmCode """
lightOne renderConfig =
    Badge.""" ++ variation ++ """Light "123"
      |> Badge.renderElement renderConfig

darkOne renderConfig =
    Badge.""" ++ variation ++ """Dark "456"
      |> Badge.renderElement renderConfig
"""
