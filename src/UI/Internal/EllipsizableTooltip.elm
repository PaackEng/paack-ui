module UI.Internal.EllipsizableTooltip exposing (EllipsisHelper, view)

import Element exposing (Element, fill, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Keyed as Keyed
import UI.Icon as Icon
import UI.Internal.Basics exposing (assertToMaybe, ifThenElse)
import UI.Internal.Palette as Palette
import UI.Internal.Utils.Element exposing (overlay, positionFixed, zIndex)
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text exposing (Text)
import UI.Utils.Element as Element


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
        , Element.inFront (tooltip renderConfig helper.collapse text helper.expanded)
        , Element.pointer
        ]
        [ ( "short", short renderConfig text )
        , ( "toggle"
          , not helper.expanded
                |> assertToMaybe helper.expand
                |> toggle renderConfig
          )
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
            ]


tooltip : RenderConfig -> msg -> Text -> Bool -> Element msg
tooltip renderConfig collapseMsg text visible =
    if visible then
        tooltipBalloon renderConfig collapseMsg text

    else
        Element.none


tooltipBalloon : RenderConfig -> msg -> Text -> Element msg
tooltipBalloon renderConfig collapseMsg text =
    overlay collapseMsg <|
        Keyed.row
            [ Background.color Palette.gray.lighter
            , Border.rounded 8
            , Element.paddingXY 8 8
            , zIndex 8
            , positionFixed
            ]
            [ ( "text"
              , text
                    |> Text.setEllipsis False
                    |> Text.renderElement renderConfig
              )
            ]
