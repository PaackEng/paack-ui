module Badges exposing (stories)

import Element exposing (Element)
import PluginOptions exposing (defaultWithMenu)
import UI.Badge as Badge exposing (Badge)
import UI.RenderConfig exposing (RenderConfig)
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, goToDocsCallToAction, prettifyElmCode, story, storyList)
import UI.Icon as Icon
import Utils exposing (iconsSvgSprite)

stories : RenderConfig -> ExplorerUI
stories cfg =
    storiesOf
        "Badges"
        [ oneBadge cfg Badge.grayLight Badge.grayDark "gray"
        , oneBadge cfg Badge.primaryLight Badge.primaryDark "primary"
        , oneBadge cfg Badge.warningLight Badge.warningDark "warning"
        , oneBadge cfg Badge.dangerLight Badge.dangerDark "danger"
        , oneBadge cfg Badge.successLight Badge.successDark "success"
        , oneBadge cfg Badge.outlineLight Badge.outlineDark "outline"
        , allBadge cfg
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
            [ 
             constructorLight "123"
            , Badge.grayLightWithIcon "123" (Icon.search "search")
            , constructorDark "456"
            , Badge.outlineDarkWithIcon "123" (Icon.search "search")
            ]

            |> (::) iconsSvgSprite
        , { defaultWithMenu
            | code = code variation
            , note = goToDocsCallToAction "Badge"
          }
        )

allBadge : RenderConfig -> ExplorerStory
allBadge cfg =
    story
        ( "United"
        , [ [ Badge.primaryLight, Badge.primaryDark ]
          , [ Badge.warningLight, Badge.warningDark ]
          , [ Badge.dangerLight, Badge.dangerDark ]
          , [ Badge.successLight, Badge.successDark ]
          , [ Badge.outlineLight, Badge.outlineDark ]
          ]
            |> List.map (uniteVariation cfg)
            |> Element.column [ Element.spacing 8 ]
        , defaultWithMenu
        )


uniteVariation : RenderConfig -> List (String -> Badge) -> Element msg
uniteVariation cfg constructors =
    constructors
        |> List.map (\constructor -> Badge.renderElement cfg (constructor "987"))
        |> Element.row [ Element.spacing 8 ]


code : String -> String
code variation =
    prettifyElmCode <|
        """lightOne renderConfig =
    Badge."""
            ++ variation
            ++ """Light "123"
      |> Badge.renderElement renderConfig

darkOne renderConfig =
    Badge."""
            ++ variation
            ++ """Dark "456"
      |> Badge.renderElement renderConfig
"""
