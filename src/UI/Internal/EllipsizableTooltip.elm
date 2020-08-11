module UI.Internal.EllipsizableTooltip exposing (view)

import Element exposing (Element, fill, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Keyed as Keyed
import UI.Icon as Icon
import UI.Internal.Palette as Palette
import UI.Internal.Utils.Element exposing (positionFixed, zIndex)
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text exposing (Text)
import UI.Utils.Element exposing (zeroPadding)


view : RenderConfig -> Text -> Element msg
view renderConfig text =
    Keyed.row
        [ Element.width fill
        , Element.height shrink
        , Element.inFront (focusable renderConfig text)
        ]
        [ ( "short", short renderConfig text )
        , ( "toggle", toggle renderConfig True )
        ]


focusable : RenderConfig -> Text -> Element msg
focusable renderConfig text =
    Keyed.row
        [ Element.width fill
        , Element.height shrink
        , Border.rounded 8
        , Element.alpha 0.0
        , Element.mouseDown
            [ Element.alpha 1.0 ]
        , Element.focused
            [ Element.alpha 1.0 ]
        , Element.mouseOver
            [ Element.alpha 1.0 ]
        , Background.color Palette.gray.lightest
        ]
        [ ( "short", short renderConfig text )
        , ( "toggle", toggle renderConfig False )
        ]


short : RenderConfig -> Text -> Element msg
short renderConfig text =
    text
        |> Text.setEllipsis True
        |> Text.renderElement renderConfig
        |> Element.el
            [ Element.width fill
            , Element.clipX
            , Element.spacing 14
            ]


toggle : RenderConfig -> Bool -> Element msg
toggle renderConfig invisible =
    Icon.seeMore "View more"
        |> Icon.withSize Size.extraSmall
        |> Icon.renderElement renderConfig
        |> Element.el
            [ Element.padding 4
            , Border.rounded 20
            , Background.color Palette.gray.lighter
            , Element.pointer
            ]
        |> Element.el
            [ Element.transparent invisible
            , Element.paddingXY 10 0
            , Element.centerY
            ]


tooltip : RenderConfig -> Text -> Bool -> Element msg
tooltip renderConfig text visible =
    Keyed.el
        [ Element.width fill
        , Element.paddingEach { zeroPadding | top = 32 }
        , positionFixed
        , zIndex 8
        , Background.color Palette.gray.lightest
        ]
    <|
        if visible then
            ( "visible"
            , text
                |> Text.setEllipsis False
                |> Text.renderElement renderConfig
            )

        else
            ( "invisible", Element.none )
