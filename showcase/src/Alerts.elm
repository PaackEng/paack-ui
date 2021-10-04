module Alerts exposing (stories)

import Element exposing (Element, fill)
import PluginOptions exposing (PluginOptions, defaultWithMenu)
import UI.Alert as Alert
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, goToDocsCallToAction, iconsSvgSprite, prettifyElmCode, story)
import UI.Text as Text
import UI.Palette as Palette

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
        , Element.column [ Element.width fill, Element.spacing 40 ]
            [   Element.column [ Element.width fill, Element.spacing 20 ][
                    Element.column [ Element.width fill, Element.spacing 8 ]
                        [ Text.overline "FULL BLEED ALERTS"
                            |>Text.withColor Palette.gray700
                            |> Text.renderElement renderConfig
                        ]
                    ,Element.column [ Element.width fill, Element.spacing 8 ]
                    [ iconsSvgSprite
                    , alert Alert.primary renderConfig
                    , alert Alert.success renderConfig
                    , alert Alert.warning renderConfig
                    , alert Alert.danger renderConfig
                    , alertWithIcon Alert.success renderConfig
                    , alertWithIcon Alert.warning renderConfig
                    , alertWithIcon Alert.danger renderConfig
                    ]
                ]
                ,Element.column [ Element.width fill, Element.spacing 20 ]
                [   Element.column [ Element.width fill, Element.spacing 8 ]
                    [ Text.overline "INLINE ALERTS"
                        |>Text.withColor Palette.gray700
                        |> Text.renderElement renderConfig
                    ]
                    ,Element.column [ Element.width fill, Element.spacing 8 ]
                    [ iconsSvgSprite
                    , inlineAlert Alert.primary renderConfig
                    , inlineAlert Alert.success renderConfig
                    , inlineAlert Alert.warning renderConfig
                    , inlineAlert Alert.danger renderConfig
                    , inlineAlertWithGenericIcon Alert.success renderConfig
                    , inlineAlertWithGenericIcon Alert.warning renderConfig
                    , inlineAlertWithGenericIcon Alert.danger renderConfig
                    ]
                ]
            ]
        , defaultWithMenu
        )


alert : (String -> Alert.Alert msg) -> RenderConfig -> Element msg
alert alertFn renderConfig =
    alertFn "Lorem ipsum dolor sit amet."
        |> Alert.renderElement renderConfig


alertWithIcon : (String -> Alert.Alert msg) -> RenderConfig -> Element msg
alertWithIcon alertFn renderConfig =
    alertFn "I have an icon."
        |> Alert.withGenericIcon
        |> Alert.renderElement renderConfig

inlineAlert : (String -> Alert.Alert msg) -> RenderConfig -> Element msg
inlineAlert alertFn renderConfig =
    alertFn "I'm an inline alert !"
        |> Alert.isInline
        |> Alert.renderElement renderConfig

inlineAlertWithGenericIcon : (String -> Alert.Alert msg) -> RenderConfig -> Element msg
inlineAlertWithGenericIcon alertFn renderConfig =
    alertFn "I'm an inline alert !"
        |> Alert.isInline
        |> Alert.withGenericIcon
        |> Alert.renderElement renderConfig


inlineAlert : (String -> Alert.Alert msg) -> RenderConfig -> Element msg
inlineAlert alertFn renderConfig =
    alertFn "I'm an inline alert !"
        |> Alert.withInlineStyle
        |> Alert.renderElement renderConfig


inlineAlertWithGenericIcon : (String -> Alert.Alert msg) -> RenderConfig -> Element msg
inlineAlertWithGenericIcon alertFn renderConfig =
    alertFn "I'm an inline alert !"
        |> Alert.withInlineStyle
        |> Alert.withGenericIcon
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
