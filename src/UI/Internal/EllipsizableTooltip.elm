module UI.Internal.EllipsizableTooltip exposing (view)

import Element exposing (Element, fill, shrink)
import Element.Background as Background
import Element.Border as Border
import UI.Internal.Palette as Palette
import UI.Internal.Text as InternalText
import UI.Internal.Utils.Element as InternalElement exposing (tabIndex, zIndex)
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text exposing (Text)
import UI.Utils.Element as Element exposing (zeroPadding)


view : RenderConfig -> Text -> Element msg
view renderConfig text =
    text
        |> textView renderConfig
        |> Element.el
            [ Element.width fill
            , Element.height shrink
            , Element.inFront (focusableView renderConfig text)
            ]


focusableView : RenderConfig -> Text -> Element msg
focusableView renderConfig text =
    text
        |> textView renderConfig
        |> Element.el
            [ Element.width fill
            , Element.height shrink
            , Border.rounded 8
            , Element.alpha 0.0
            , Element.mouseOver
                [ Element.alpha 1.0 ]
            , tabIndex 0
            , Element.pointer
            , Element.inFront (tooltip renderConfig text)
            ]


textView : RenderConfig -> Text -> Element msg
textView renderConfig text =
    text
        |> Text.setEllipsis True
        |> Text.renderElement renderConfig
        |> Element.el
            [ Element.width fill
            , Element.clipX
            , Element.paddingXY 8 0
            , Element.centerY
            ]


tooltipBalloon : RenderConfig -> Text -> Element msg
tooltipBalloon renderConfig text =
    text
        |> Text.setEllipsis False
        |> Text.renderElement renderConfig
        |> Element.el
            [ Background.color Palette.gray.lighter
            , Border.rounded 8
            , Element.paddingXY 8 8
            , tabIndex -1
            , zIndex 1
            , InternalElement.positionAbsolute
            , InternalElement.positionAbsoluteTop
            , InternalElement.positionAbsoluteLeft
            ]


tooltip : RenderConfig -> Text -> Element msg
tooltip renderConfig text =
    text
        |> tooltipBalloon renderConfig
        |> Element.el
            [ Element.paddingEach
                { zeroPadding
                    | top = InternalText.textSizePx renderConfig text
                }
            , Element.width fill
            , Element.height fill
            , InternalElement.positionRelative
            , InternalElement.overflowVisible
            ]
