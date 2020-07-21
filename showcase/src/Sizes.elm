module Sizes exposing (stories)

import Element
import Element.Background as Background
import Element.Font as Font
import Msg as Msg
import PluginOptions exposing (defaultWithMenu)
import UI.Button as Button
import UI.Icon as Icon
import UI.Internal.Palette as Palette
import UI.Size as Size
import UI.TextField as TextField
import UIExplorer exposing (storiesOf)
import Utils exposing (iconsSvgSprite, prettifyElmCode)


stories cfg =
    storiesOf
        "Sizes"
        [ ( "Large"
          , \_ -> sizeView cfg Size.large
          , { defaultWithMenu
                | note = notes "large"
            }
          )
        , ( "Medium"
          , \_ -> sizeView cfg Size.medium
          , { defaultWithMenu
                | note = notes "medium"
            }
          )
        , ( "Small"
          , \_ -> sizeView cfg Size.small
          , { defaultWithMenu
                | note = notes "small"
            }
          )
        , ( "ExtraSmall"
          , \_ -> sizeView cfg Size.extraSmall
          , { defaultWithMenu
                | note = notes "extraSmall"
            }
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


notes suffix =
    """To achieve this size use:

* Buttons
```elm
--...
    |> Button.withSize Size.""" ++ suffix ++ """
    -- ...
```

* Icons
```elm
--...
    |> Icon.withSize Size.""" ++ suffix ++ """
    -- ...
```
  """
