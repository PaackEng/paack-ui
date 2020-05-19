module UI.Internal.Dialog exposing (Dialog, dialogMap, view)

import Element exposing (Element, fill, maximum)
import Element.Background as Background
import Element.Events as Events
import UI.Icon as Icon
import UI.Internal.Basics exposing (lazyMap)
import UI.Internal.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.Element as Element


type alias Dialog msg =
    { title : String
    , close : msg
    , body : RenderConfig -> Element msg
    }


dialogMap : (a -> b) -> Dialog a -> Dialog b
dialogMap applier data =
    { title = data.title
    , close = applier data.close
    , body = lazyMap (Element.map applier) data.body
    }


view : RenderConfig -> Dialog msg -> Element msg
view cfg { title, body, close } =
    Element.row
        [ Element.width fill
        , Element.height fill
        , Palette.gray.darkest
            |> Element.colorWithOpacity 0.85
            |> Background.color
        ]
        [ Element.column
            [ Element.width (fill |> maximum 640)
            , Element.centerX
            , Element.centerY
            , Background.color <| Palette.gray.lightest
            ]
            [ Element.row [ Element.width fill ]
                [ Text.heading5 title
                    |> Text.toEl cfg
                    |> Element.el [ Element.width fill ]
                , Icon.close "Close dialog"
                    |> Icon.toEl cfg
                    |> Element.el
                        [ Events.onClick close
                        , Element.pointer
                        ]
                ]
            , body cfg
            ]
        ]
