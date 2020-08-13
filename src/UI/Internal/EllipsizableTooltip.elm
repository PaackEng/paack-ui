module UI.Internal.EllipsizableTooltip exposing (view)

import Element exposing (Element, fill, px, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Keyed as Keyed
import UI.Icon as Icon
import UI.Internal.Palette as Palette
import UI.Internal.Text as InternalText
import UI.Internal.Utils.Element as InternalElement exposing (borderTriangleUp, tabIndex, zIndex)
import UI.Palette as Palette exposing (brightnessLight, brightnessLighter, toneGray)
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text exposing (Text)
import UI.Utils.Element as Element exposing (zeroPadding)


view : RenderConfig -> Text -> Element msg
view renderConfig text =
    Keyed.row
        [ Element.width fill
        , Element.height shrink
        , Element.inFront (focusableView renderConfig text)
        ]
        [ ( "short", short renderConfig text )
        , ( "toggle", toggle renderConfig True )
        ]


focusableView : RenderConfig -> Text -> Element msg
focusableView renderConfig text =
    Keyed.row
        [ Element.width fill
        , Element.height shrink
        , Border.rounded 8
        , Element.alpha 0.0
        , Element.mouseOver
            [ Element.alpha 1.0 ]
        , Element.focused
            [ Element.alpha 1.0 ]
        , tabIndex 0
        , Element.pointer
        , Background.color Palette.gray.lightest
        , Element.inFront (tooltip renderConfig text)
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
            , Element.paddingXY 8 0
            , Element.centerY
            ]



--, Element.inFront (tooltipHelper renderConfig helper.collapse text helper.expanded)


toggle : RenderConfig -> Bool -> Element msg
toggle renderConfig transparent =
    Icon.seeMore "View more"
        |> Icon.withSize Size.extraSmall
        |> Icon.withColor (Palette.color toneGray brightnessLight |> Palette.setContrasting True)
        |> Icon.renderElement renderConfig
        |> Element.el
            [ Element.padding 4
            , Border.rounded 20
            , Background.color Palette.gray.light
            ]
        |> Element.el
            [ Element.transparent transparent
            , Element.paddingXY 8 4
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
            , Element.inFront arrow
            , InternalElement.positionRelative
            , InternalElement.overflowVisible
            ]


arrow : Element msg
arrow =
    Element.column
        [ Element.width fill
        , Element.alignBottom
        ]
        [ Element.el
            [ Element.paddingXY 12 0
            , Element.alignRight
            ]
          <|
            Element.el
                [ Element.width (px 0)
                , Element.height (px 0)
                , Border.widthEach { top = 0, left = 5, right = 5, bottom = 5 }
                , borderTriangleUp (Palette.toCssColor <| Palette.color toneGray brightnessLighter)
                , zIndex 8
                ]
                Element.none
        ]
