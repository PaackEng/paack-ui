module Badges exposing (stories)

import Element exposing (Element)
import PluginOptions exposing (defaultWithMenu)
import UI.Badge as Badge exposing (Badge)
import UI.Icon as Icon
import UI.RenderConfig exposing (RenderConfig)
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, goToDocsCallToAction, iconsSvgSprite, prettifyElmCode, story, storyList)


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
            [ constructorLight "123"
            , constructorLight "Car" |> Badge.withIcon (Icon.car "Car")
            , constructorDark "456"
            , constructorDark "Bicycle" |> Badge.withIcon (Icon.bicycle "Bicycle")
            ]
            |> (::) iconsSvgSprite
        , { defaultWithMenu
            | code = code variation
            , note = goToDocsCallToAction "Badge"
          }
        )


allBadge : RenderConfig -> ExplorerStory
allBadge cfg =
    let
        variants =
            [ [ Badge.primaryLight, Badge.primaryDark ]
            , [ Badge.warningLight, Badge.warningDark ]
            , [ Badge.dangerLight, Badge.dangerDark ]
            , [ Badge.successLight, Badge.successDark ]
            , [ Badge.outlineLight, Badge.outlineDark ]
            ]
    in
    story
        ( "United"
        , Element.row [ Element.spacing 24 ]
            [ iconsSvgSprite
            , variants
                |> List.map (uniteVariation cfg)
                |> Element.column [ Element.spacing 12 ]
            , variants
                |> List.map (uniteVariationWithIcon cfg)
                |> Element.column [ Element.spacing 12 ]
            ]
        , defaultWithMenu
        )


uniteVariation : RenderConfig -> List (String -> Badge) -> Element msg
uniteVariation cfg constructors =
    constructors
        |> List.map
            (\constructor ->
                Badge.renderElement cfg (constructor "987")
            )
        |> Element.row [ Element.spacing 8 ]


uniteVariationWithIcon : RenderConfig -> List (String -> Badge) -> Element msg
uniteVariationWithIcon cfg constructors =
    constructors
        |> List.map
            (\constructor ->
                (constructor "12"
                    |> Badge.withIcon (Icon.packages "Packages")
                )
                    |> Badge.renderElement cfg
            )
        |> Element.row [ Element.spacing 8 ]


code : String -> String
code variation =
    prettifyElmCode <|
        """lightOne renderConfig =
    Badge."""
            ++ variation
            ++ """Light "123"
      |> Badge.renderElement renderConfig

lightOneWithIcon renderConfig =
    Badge."""
            ++ variation
            ++ """Light "Car"
      |> Badge.withIcon (Icon.car "Car")
      |> Badge.renderElement renderConfig

darkOne renderConfig =
    Badge."""
            ++ variation
            ++ """Dark "456"
      |> Badge.renderElement renderConfig

darkOneWithIcon renderConfig =
    Badge."""
            ++ variation
            ++ """Dark "Bicycle"
      |> Badge.withIcon (Icon.bicycle "Bicycle")
      |> Badge.renderElement renderConfig
"""
