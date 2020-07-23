module Sizes exposing (stories)

import Element
import Element.Background as Background
import Element.Font as Font
import Msg as Msg
import PluginOptions exposing (PluginOptions, defaultWithMenu)
import UI.Button as Button
import UI.Icon as Icon
import UI.Internal.Palette as Palette
import UI.Size as Size
import UI.TextField as TextField
import UIExplorer exposing (storiesOf)
import Utils exposing (goToDocsCallToAction, iconsSvgSprite, prettifyElmCode)


stories cfg =
    storiesOf
        "Sizes"
        [ ( "Large"
          , \_ -> sizeView cfg Size.large
          , pluginOptions "large"
          )
        , ( "Medium"
          , \_ -> sizeView cfg Size.medium
          , pluginOptions "medium"
          )
        , ( "Small"
          , \_ -> sizeView cfg Size.small
          , pluginOptions "small"
          )
        , ( "ExtraSmall"
          , \_ -> sizeView cfg Size.extraSmall
          , pluginOptions "extraSmall"
          )
        ]


icons =
    [ ( Icon.close, "Close" )
    , ( Icon.toggle, "Toggle" )
    ]


iconView cfg size ( iconFn, label ) =
    Element.column
        [ Background.color Palette.gray.darkest
        , Font.color Palette.darkConstrast.darkest
        , Element.spacing 10
        , Element.padding 10
        ]
        [ iconFn label
            |> Icon.withSize size
            |> Icon.renderElement cfg
            |> Element.el
                [ Font.size 25
                , Element.centerX
                , Background.color <| Palette.primary.middle
                ]
        , Element.el [ Font.size 14 ] <| Element.text label
        ]


buttons =
    [ Button.fromLabel "Prompt"
    , Button.fromIcon <| Icon.toggle "Toggle"
    ]


buttonView cfg size body =
    body
        |> Button.cmd Msg.NoOp Button.primary
        |> Button.withSize size
        |> Button.renderElement cfg


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
        |> Element.layout []


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
