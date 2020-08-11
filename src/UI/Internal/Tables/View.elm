module UI.Internal.Tables.View exposing (..)

import Element exposing (Attribute, Element, fill, fillPortion, minimum, px, shrink)
import Element.Background as Background
import Element.Border as Border
import UI.Button as Button
import UI.Internal.EllipsizableTooltip as EllipsizableTooltip
import UI.Internal.NArray as NArray
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Internal.Tables.Common exposing (..)
import UI.Internal.Text as InternalText
import UI.Palette as Palette exposing (brightnessMiddle, toneGray)
import UI.RenderConfig exposing (RenderConfig)
import UI.Tables.Common as Common exposing (..)
import UI.Text as Text exposing (Text)


cellContentRender : RenderConfig -> Int -> Int -> Common.Cell msg -> Element msg
cellContentRender renderConfig row col cell_ =
    case cell_ of
        CellText text ->
            simpleText renderConfig text

        CellEllipsizableText chars text ->
            if InternalText.length text > chars then
                ellipsisText renderConfig text

            else
                simpleText renderConfig text

        CellButton button ->
            Button.renderElement renderConfig button

        CellCustom element ->
            element


simpleText : RenderConfig -> Text -> Element msg
simpleText renderConfig text =
    text
        |> Text.setEllipsis True
        |> Text.renderElement renderConfig
        |> Element.el
            [ Element.width fill
            , Element.clipX
            , Element.paddingXY 8 4
            ]


ellipsisText : RenderConfig -> Text -> Element msg
ellipsisText renderConfig text =
    text
        |> EllipsizableTooltip.view renderConfig
        |> Element.el
            [ Element.width fill
            , Element.clipX
            , Element.paddingXY 8 4
            ]


widthToEl : Common.ColumnWidth -> Element.Length
widthToEl width =
    case width of
        WidthPortion int ->
            fillPortion int

        WidthPixels int ->
            px int


rowRender : RenderConfig -> ToRow msg item columns -> Int -> List Column -> item -> List (Element msg)
rowRender renderConfig toRow row columns item =
    toRow item
        |> NArray.toList
        |> List.map2 (\col cell_ i -> cellRender renderConfig row i col cell_) columns
        |> List.indexedMap (\i renderer -> renderer i)


rowBox : List (Element msg) -> Element msg
rowBox cells =
    Element.row
        [ Element.spacing 8
        , Primitives.defaultRoundedBorders
        , Element.width fill
        , Element.mouseOver [ Background.color Palette.gray.lightest ]
        ]
        cells


cellRender : RenderConfig -> Int -> Int -> Column -> Common.Cell msg -> Element msg
cellRender renderConfig row col (Column _ { width }) cell =
    cell
        |> cellContentRender renderConfig row col
        |> cellSpace width


cellSpace : Common.ColumnWidth -> Element msg -> Element msg
cellSpace width =
    Element.el
        [ Element.width (widthToEl width)
        , Element.height (shrink |> minimum 1)
        , Element.centerY
        ]


topCellSpace : Common.ColumnWidth -> Element msg -> Element msg
topCellSpace width =
    Element.el
        [ Element.width (widthToEl width)
        , Element.height (shrink |> minimum 1)
        , Element.alignTop
        ]


simpleHeaderRender : RenderConfig -> String -> Element msg
simpleHeaderRender renderConfig header =
    header
        |> String.toUpper
        |> Text.overline
        |> Text.withColor (Palette.color toneGray brightnessMiddle)
        |> cellFromText
        |> cellContentRender renderConfig -1 -1


headersAttr : List (Attribute msg)
headersAttr =
    [ Element.spacing 8
    , Element.width fill
    , Element.paddingEach { bottom = 7, top = 0, left = 0, right = 0 }
    , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
    , Border.color Palette.gray.lightest
    ]
