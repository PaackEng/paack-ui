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
        , allBadgesWithIcon cfg
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
            , constructorDark "456"
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


allBadgesWithIcon : RenderConfig -> ExplorerStory
allBadgesWithIcon cfg =
    story
        ( "United, With Icon"
        , Element.column
            [ Element.spacing 12 ]
            [ iconsSvgSprite
            , Element.row [ Element.spacing 8 ]
                [ Badge.primaryLightWithIcon "Car" (Icon.car "car") |> Badge.renderElement cfg
                , Badge.primaryDarkWithIcon "Car" (Icon.car "car") |> Badge.renderElement cfg
                ]
            , Element.row [ Element.spacing 8 ]
                [ Badge.warningLightWithIcon "Car" (Icon.car "car") |> Badge.renderElement cfg
                , Badge.warningDarkWithIcon "Car" (Icon.car "car") |> Badge.renderElement cfg
                ]
            , Element.row [ Element.spacing 8 ]
                [ Badge.dangerLightWithIcon "Car" (Icon.car "car") |> Badge.renderElement cfg
                , Badge.dangerDarkWithIcon "Car" (Icon.car "car") |> Badge.renderElement cfg
                ]
            , Element.row [ Element.spacing 8 ]
                [ Badge.successLightWithIcon "Car" (Icon.car "car") |> Badge.renderElement cfg
                , Badge.successDarkWithIcon "Car" (Icon.car "car") |> Badge.renderElement cfg
                ]
            , Element.row [ Element.spacing 8 ]
                [ Badge.grayLightWithIcon "Car" (Icon.car "car") |> Badge.renderElement cfg
                , Badge.grayDarkWithIcon "Car" (Icon.car "car") |> Badge.renderElement cfg
                ]
            , Element.row [ Element.spacing 8 ]
                [ Badge.outlineLightWithIcon "Car" (Icon.car "car") |> Badge.renderElement cfg
                , Badge.outlineDarkWithIcon "Car" (Icon.car "car") |> Badge.renderElement cfg
                ]
            ]
        , { defaultWithMenu
            | code =
                prettifyElmCode <|
                    """lightOneWithIcon renderConfig =
    Badge.grayLightWithIcon "Car" (Icon.car "car")
    |> Badge.renderElement renderConfig"""
                        ++ """
    """
                        ++ """
darkOneWithIcon renderConfig =
    Badge.grayDarkWithIcon "Car" (Icon.car "car")
    |> Badge.renderElement renderConfig

"""
            , note = goToDocsCallToAction "Badge"
          }
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
