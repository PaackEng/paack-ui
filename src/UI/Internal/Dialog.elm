module UI.Internal.Dialog exposing (Dialog, dialogMap, view)

import Element exposing (Element, fill, maximum, px)
import Element.Background as Background
import Element.Events as Events
import UI.Icon as Icon
import UI.Internal.Basics exposing (lazyMap)
import UI.Internal.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.ARIA as ARIA exposing (roleButton)
import UI.Utils.Element as Element


type alias Dialog msg =
    { title : String
    , close : msg
    , body : Element msg
    }


dialogMap : (a -> b) -> Dialog a -> Dialog b
dialogMap applier data =
    { title = data.title
    , close = applier data.close
    , body = Element.map applier data.body
    }


view : RenderConfig -> Dialog msg -> Element msg
view cfg { title, body, close } =
    Element.row
        [ Element.width fill
        , Element.height fill
        , Palette.gray.darkest
            |> Element.colorSetOpacity 0.85
            |> Background.color
        ]
        [ Element.column
            [ Element.centerX
            , Element.centerY
            , Element.padding 12
            , Background.color <| Palette.gray.lightest
            ]
            [ Element.row [ Element.width fill ]
                [ Text.heading5 title
                    |> Text.toEl cfg
                    |> Element.el
                        [ Element.width fill
                        , Element.paddingEach { top = 20, left = 20, right = 0, bottom = 0 }
                        ]
                , Icon.close "Close dialog"
                    |> Icon.toEl cfg
                    |> Element.el
                        [ Events.onClick close
                        , Element.pointer
                        , Element.width (px 14)
                        , Element.paddingXY 26 20
                        , ARIA.roleAttr ARIA.roleButton
                        ]
                ]
            , body
            ]
        ]
