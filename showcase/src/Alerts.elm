module Alerts exposing (stories)

import Element exposing (Element, fill)
import PluginOptions exposing (PluginOptions, defaultWithMenu)
import UI.Alert as Alert
import UI.RenderConfig exposing (RenderConfig)
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, goToDocsCallToAction, prettifyElmCode, story)


stories : RenderConfig -> ExplorerUI
stories renderConfig =
    storiesOf
        "Alerts"
        [ primaryStory renderConfig
        , successStory renderConfig
        , warningStory renderConfig
        , dangerStory renderConfig
        , unitedStory renderConfig
        ]


primaryStory : RenderConfig -> ExplorerStory
primaryStory renderConfig =
    story
        ( "Primary"
        , alert Alert.primary renderConfig
        , pluginOptions "primary"
        )


successStory : RenderConfig -> ExplorerStory
successStory renderConfig =
    story
        ( "Success"
        , alert Alert.success renderConfig
        , pluginOptions "success"
        )


warningStory : RenderConfig -> ExplorerStory
warningStory renderConfig =
    story
        ( "Warning"
        , alert Alert.warning renderConfig
        , pluginOptions "warning"
        )


dangerStory : RenderConfig -> ExplorerStory
dangerStory renderConfig =
    story
        ( "Danger"
        , alert Alert.danger renderConfig
        , pluginOptions "danger"
        )


unitedStory : RenderConfig -> ExplorerStory
unitedStory renderConfig =
    story
        ( "United"
        , Element.column [ Element.width fill, Element.spacing 8 ]
            [ alert Alert.primary renderConfig
            , alert Alert.success renderConfig
            , alert Alert.warning renderConfig
            , alert Alert.danger renderConfig
            ]
        , defaultWithMenu
        )


alert : (String -> Alert.Alert msg) -> RenderConfig -> Element msg
alert alertFn renderConfig =
    alertFn "Lorem ipsum dolor sit amet."
        |> Alert.renderElement renderConfig


pluginOptions : String -> PluginOptions
pluginOptions alertType =
    { defaultWithMenu
        | code = code alertType
        , note = goToDocsCallToAction "Alert"
    }


code : String -> String
code alertType =
    prettifyElmCode <|
        """Alert."""
            ++ alertType
            ++ """ "Hey I just met you"
    |> Alert.toEl
"""
