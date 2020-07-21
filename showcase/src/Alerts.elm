module Alerts exposing (stories)

import Element exposing (..)
import Element.Background as Background
import Msg as Msg
import PluginOptions exposing (PluginOptions, defaultWithMenu)
import UI.Alert as Alert
import UI.RenderConfig exposing (RenderConfig)
import UIExplorer exposing (storiesOf)
import Utils exposing (goToDocsCallToAction, prettifyElmCode, story)


stories renderConfig =
    storiesOf
        "Alerts"
        [ primaryStory renderConfig
        , successStory renderConfig
        , warningStory renderConfig
        , dangerStory renderConfig
        ]


primaryStory renderConfig =
    story
        ( "Primary"
        , alert Alert.primary renderConfig
        , pluginOptions "primary"
        )


successStory renderConfig =
    story
        ( "Success"
        , alert Alert.success renderConfig
        , pluginOptions "success"
        )


warningStory renderConfig =
    story
        ( "Warning"
        , alert Alert.warning renderConfig
        , pluginOptions "warning"
        )


dangerStory renderConfig =
    story
        ( "Danger"
        , alert Alert.danger renderConfig
        , pluginOptions "danger"
        )


alert : (String -> Alert.Alert msg) -> RenderConfig -> Element msg
alert alertFn renderConfig =
    alertFn "Hey I just met you"
        |> Alert.renderElement renderConfig


pluginOptions : String -> PluginOptions
pluginOptions alertType =
    { defaultWithMenu
        | code =
            prettifyElmCode <|
                """
Alert."""
                    ++ alertType
                    ++ """"Hey I just met you"
    |> Alert.toEl
"""
        , note = goToDocsCallToAction "Alert"
    }
