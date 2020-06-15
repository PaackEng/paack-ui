module Sizes exposing (stories)

import Element
import Element.Background as Background
import Element.Font as Font
import Msg as Msg
import UI.Button as Button
import UI.Icon as Icon
import UI.Internal.Palette as Palette
import UI.Size as Size
import UIExplorer exposing (storiesOf)


stories cfg =
    storiesOf
        "Sizes"
        [ ( "Large"
          , \_ -> sizeView cfg Size.large
          , { note = "" }
          )
        , ( "Medium"
          , \_ -> sizeView cfg Size.medium
          , { note = "" }
          )
        , ( "Small"
          , \_ -> sizeView cfg Size.small
          , { note = "" }
          )
        , ( "ExtraSmall"
          , \_ -> sizeView cfg Size.extraSmall
          , { note = "" }
          )
        ]


icons =
    [ ( Icon.close, "Close" )
    , ( Icon.toggle, "Toggle" )
    ]


iconView cfg size ( iconFn, label ) =
    Element.column
        [ Background.color Palette.gray.darkest
        , Font.color Palette.contrastGray.darkest
        , Element.spacing 10
        , Element.padding 10
        ]
        [ iconFn label
            |> Icon.withSize size
            |> Icon.toEl cfg
            |> Element.el
                [ Font.size 25
                , Element.centerX
                , Background.color <| Palette.primary.middle
                ]
        , Element.el [ Font.size 14 ] <| Element.text label
        ]


buttons =
    [ Button.bodyText "Prompt"
    , Button.bodyIcon <| Icon.toggle "Toggle"
    ]


buttonView cfg size body =
    body
        |> Button.button Msg.NoOp
        |> Button.withSize size
        |> Button.toEl cfg


sizeView cfg size =
    [ List.map (iconView cfg size) icons
    , List.map (buttonView cfg size) buttons
    ]
        |> List.concat
        |> Element.wrappedRow
            [ Element.spacing 10
            ]
        |> Element.layout []
