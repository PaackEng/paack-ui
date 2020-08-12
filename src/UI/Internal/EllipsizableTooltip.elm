module UI.Internal.EllipsizableTooltip exposing (EllipsisHelper, view)

import Element exposing (Element, fill, px, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Keyed as Keyed
import UI.Icon as Icon
import UI.Internal.Basics exposing (ifThenElse)
import UI.Internal.Palette as Palette
import UI.Internal.Text as InternalText
import UI.Internal.Utils.Element exposing (borderTriangleUp, overlay, positionFixed, zIndex)
import UI.Palette as Palette exposing (brightnessLighter, toneGray)
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text exposing (Text)
import UI.Utils.Element as Element exposing (zeroPadding)


type alias EllipsisHelper msg =
    { expand : msg
    , collapse : msg
    , expanded : Bool
    }


view : RenderConfig -> EllipsisHelper msg -> Text -> Element msg
view renderConfig helper text =
    Keyed.row
        [ Element.width fill
        , Element.height shrink
        , Element.inFront (focusable renderConfig helper text)
        ]
        [ ( "short", short renderConfig text )
        , ( "toggle", toggle renderConfig Nothing )
        ]


focusable : RenderConfig -> EllipsisHelper msg -> Text -> Element msg
focusable renderConfig helper text =
    Keyed.row
        [ Element.width fill
        , Element.height shrink
        , Border.rounded 8
        , Element.alpha (ifThenElse helper.expanded 1.0 0.0)
        , Element.mouseDown
            [ Element.alpha 1.0 ]
        , Element.mouseOver
            [ Element.alpha 1.0 ]
        , Background.color Palette.gray.lightest
        , Element.inFront (tooltipHelper renderConfig helper.collapse text helper.expanded)
        ]
        [ ( "short", short renderConfig text )
        , ( "toggle", toggle renderConfig (Just helper.expand) )
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


toggle : RenderConfig -> Maybe msg -> Element msg
toggle renderConfig maybeExpand =
    Icon.seeMore "View more"
        |> Icon.withSize Size.extraSmall
        |> Icon.renderElement renderConfig
        |> Element.el
            [ Element.padding 4
            , Border.rounded 20
            , Background.color Palette.gray.lighter
            ]
        |> Element.el
            [ maybeExpand
                |> Maybe.map Element.onIndividualClick
                |> Maybe.withDefault (Element.transparent True)
            , Element.paddingXY 10 0
            , Element.centerY
            , Element.pointer
            ]


tooltipHelper : RenderConfig -> msg -> Text -> Bool -> Element msg
tooltipHelper renderConfig collapseMsg text visible =
    if visible then
        tooltip renderConfig collapseMsg text

    else
        Element.none


tooltipBalloon : RenderConfig -> Text -> Element msg
tooltipBalloon renderConfig text =
    text
        |> Text.setEllipsis False
        |> Text.renderElement renderConfig
        |> Element.el
            [ Background.color Palette.gray.lighter
            , Border.rounded 8
            , Element.paddingXY 8 8
            , zIndex 8
            , positionFixed
            , Element.centerX
            ]


tooltip : RenderConfig -> msg -> Text -> Element msg
tooltip renderConfig collapseMsg text =
    text
        |> tooltipBalloon renderConfig
        |> Element.el
            [ Element.paddingEach
                { zeroPadding
                    | top = InternalText.textSizePx renderConfig text
                }
            , Element.width fill
            , Element.clipX
            , Element.inFront arrow
            ]
        |> overlay collapseMsg


arrow : Element msg
arrow =
    Element.column
        [ Element.width fill
        , Element.alignBottom
        ]
        [ Element.el
            [ Element.paddingXY 14 0
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
