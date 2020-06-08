module UI.Internal.Dialog exposing (Dialog, dialogMap, view)

import Element exposing (Element, fill, maximum, px, rgb255, shrink)
import Element.Background as Background
import Element.Events as Events
import UI.Button as Button
import UI.Icon as Icon
import UI.Internal.Basics exposing (ifThenElse, lazyMap)
import UI.Internal.Palette as Palette
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.ARIA as ARIA exposing (roleButton)
import UI.Utils.Element as Element


type alias Dialog msg =
    { title : String
    , close : msg
    , width : Element.Length
    , body : Element msg
    }


dialogMap : (a -> b) -> Dialog a -> Dialog b
dialogMap applier data =
    { title = data.title
    , close = applier data.close
    , width = data.width
    , body = Element.map applier data.body
    }


view : RenderConfig -> Dialog msg -> Element msg
view cfg { title, body, close, width } =
    let
        -- Mobile and deskop different aspects
        ( ( align, height, padding ), textPadding, bodyPadding ) =
            if RenderConfig.isMobile cfg then
                ( ( Element.alignTop, fill, 0 )
                , { top = 40, left = 20, right = 0, bottom = 0 }
                , { top = 8, left = 20, right = 20, bottom = 20 }
                )

            else
                ( ( Element.centerY, shrink, 12 )
                , { top = 20, left = 32, right = 0, bottom = 0 }
                , { top = 8, left = 32, right = 32, bottom = 32 }
                )

        headerRow =
            Element.row [ Element.width fill ]
                [ Text.heading5 title
                    |> Text.toEl cfg
                    |> Element.el
                        [ Element.width fill
                        , Element.paddingEach textPadding
                        , Element.alignTop
                        ]
                , Icon.close "Close dialog"
                    |> Button.bodyIcon
                    |> Button.button close
                    |> Button.withTone Button.toneClear
                    |> Button.toEl cfg
                ]
    in
    Element.column
        [ Element.centerX
        , align
        , Element.height height
        , Element.padding padding
        , Element.width width
        , Background.color <| rgb255 255 255 255 -- NOTE: MAIN LAYOUT'S BACKGROUND COLOR
        ]
        [ headerRow
        , body
            |> Element.el
                [ Element.width fill
                , Element.paddingEach bodyPadding
                ]
        ]
        |> blackBackground


blackBackground : Element msg -> Element msg
blackBackground =
    -- Desktop has a black background
    Element.el
        [ Element.width fill
        , Element.height fill
        , Palette.gray.darkest
            |> Element.colorSetOpacity 0.85
            |> Background.color
        ]
