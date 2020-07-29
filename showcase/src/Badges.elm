module Badges exposing (stories)

import PluginOptions exposing (defaultWithMenu)
import UI.Badge as Badge
import UIExplorer exposing (storiesOf)
import Utils exposing (goToDocsCallToAction, prettifyElmCode, storyList)


stories cfg =
    storiesOf
        "Badges"
        [ oneBadge cfg Badge.grayLight Badge.grayDark "gray"
        , oneBadge cfg Badge.primaryLight Badge.primaryDark "primary"
        , oneBadge cfg Badge.warningLight Badge.warningDark "warning"
        , oneBadge cfg Badge.dangerLight Badge.dangerDark "danger"
        , oneBadge cfg Badge.successLight Badge.successDark "success"
        ]


oneBadge cfg constructorLight constructorDark str =
    storyList
        ( "Badge " ++ str
        , [ constructorLight "123", constructorDark "456" ]
            |> List.map (Badge.renderElement cfg)
        , { defaultWithMenu
            | code = prettifyElmCode <| """
lightOne renderConfig =
    Badge.""" ++ str ++ """Light "123"
      |> Badge.renderElement renderConfig

darkOne renderConfig =
    Badge.""" ++ str ++ """Dark "456"
      |> Badge.renderElement renderConfig
"""
            , note = goToDocsCallToAction "Badge"
          }
        )
