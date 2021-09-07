module Sizes exposing (stories)

import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Msg exposing (Msg)
import PluginOptions exposing (PluginOptions, defaultWithMenu)
import UI.Button as Button exposing (ButtonBody)
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Colors as Colors
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size exposing (Size)
import UI.TextField as TextField
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerUI, goToDocsCallToAction, iconsSvgSprite, prettifyElmCode)


stories : RenderConfig -> ExplorerUI
stories cfg =
    storiesOf
        "Sizes"
        [ ( "Large"
          , \_ ->
                sizeView cfg Size.large
                    |> Element.layout []
          , pluginOptions "large"
          )
        , ( "Medium"
          , \_ ->
                sizeView cfg Size.medium
                    |> Element.layout []
          , pluginOptions "medium"
          )
        , ( "Small"
          , \_ ->
                sizeView cfg Size.small
                    |> Element.layout []
          , pluginOptions "small"
          )
        , ( "ExtraSmall"
          , \_ ->
                sizeView cfg Size.extraSmall
                    |> Element.layout []
          , pluginOptions "extraSmall"
          )
        , ( "United"
          , \_ ->
                united cfg
                    |> Element.layout []
          , defaultWithMenu
          )
        ]


united : RenderConfig -> Element Msg
united cfg =
    Element.column [ Element.spacing 8 ]
        [ sizeView cfg Size.large
        , sizeView cfg Size.medium
        , sizeView cfg Size.small
        , sizeView cfg Size.extraSmall
        ]


icons : List ( String -> Icon, String )
icons =
    [ ( Icon.close, "Close" )
    , ( Icon.toggle, "Toggle" )
    ]


iconView : RenderConfig -> Size -> ( String -> Icon, String ) -> Element msg
iconView cfg size ( iconFn, label ) =
    Element.column
        [ Background.color Colors.gray800
        , Font.color Colors.white
        , Element.spacing 10
        , Element.padding 10
        ]
        [ iconFn label
            |> Icon.withSize size
            |> Icon.renderElement cfg
            |> Element.el
                [ Font.size 25
                , Element.centerX
                , Background.color <| Colors.navyBlue700
                ]
        , Element.el [ Font.size 14 ] <| Element.text label
        ]


buttons : List ButtonBody
buttons =
    [ Button.fromLabel "Prompt"
    , Button.fromIcon <| Icon.toggle "Toggle"
    , Button.fromLabeledOnLeftIcon <| Icon.toggle "Toggle"
    ]


buttonView : RenderConfig -> Size -> ButtonBody -> Element Msg
buttonView cfg size body =
    body
        |> Button.cmd Msg.NoOp Button.primary
        |> Button.withSize size
        |> Button.renderElement cfg


sizeView : RenderConfig -> Size -> Element Msg
sizeView cfg size =
    [ List.map (iconView cfg size) icons
    , List.map (buttonView cfg size) buttons
    , [ TextField.singlelineText (always Msg.NoOp)
            "Whatever"
            ""
            |> TextField.setLabelVisible False
            |> TextField.withSize size
            |> TextField.renderElement cfg
            |> Element.el [ Element.width (Element.px 140) ]
      ]
    ]
        |> List.concat
        |> (::) iconsSvgSprite
        |> Element.wrappedRow
            [ Element.spacing 10
            ]


codeSample : String -> String
codeSample suffix =
    prettifyElmCode <|
        """
-- Buttons

submitButton =
    Button.fromLabel "Submit"
        |> Button.cmd FormSend Button.primary
        |> Button.withSize Size."""
            ++ suffix
            ++ """
        |> Button.renderElement renderConfig

-- Icons

logoutIcon =
    Icon.logout "Logout from this account"
        |> Icon.withSize Size."""
            ++ suffix
            ++ """
        |> Icon.renderElement renderConfig
"""


pluginOptions : String -> PluginOptions
pluginOptions storyType =
    { defaultWithMenu
        | code = codeSample storyType
        , note = goToDocsCallToAction "Size"
    }
