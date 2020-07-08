module UI.Internal.TableView exposing (..)

import Element exposing (Attribute, Element, fill, fillPortion, minimum, px, shrink)
import Element.Background as Background
import Element.Border as Border
import UI.Button as Button
import UI.Internal.NArray as NArray
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Internal.Table exposing (..)
import UI.Palette as Palette exposing (brightnessMiddle, toneGray)
import UI.RenderConfig exposing (RenderConfig)
import UI.Tables.Common as Common exposing (..)
import UI.Text as Text


cellContentRender : RenderConfig -> Common.Cell msg -> Element msg
cellContentRender renderConfig cell_ =
    case cell_ of
        CellText text ->
            text
                |> Text.renderElement renderConfig
                |> Element.el
                    [ Element.width fill
                    , Element.clipX
                    , Element.padding 8
                    ]

        CellButton button ->
            Button.renderElement renderConfig button


widthToEl : Common.ColumnWidth -> Element.Length
widthToEl width =
    case width of
        WidthPortion int ->
            fillPortion int

        WidthPixels int ->
            px int


rowRender : RenderConfig -> ToRow msg item columns -> List Column -> item -> Element msg
rowRender renderConfig toRow columns item =
    toRow item
        |> NArray.toList
        |> List.map2 (cellRender renderConfig) columns
        |> Element.row
            [ Element.spacing 8
            , Primitives.defaultRoundedBorders
            , Element.width fill
            , Element.mouseOver [ Background.color Palette.gray.lightest ]
            ]


cellRender : RenderConfig -> Column -> Common.Cell msg -> Element msg
cellRender renderConfig (Column _ { width }) cell =
    cell
        |> cellContentRender renderConfig
        |> cellSpace width


cellSpace : Common.ColumnWidth -> Element msg -> Element msg
cellSpace width =
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
        |> cellContentRender renderConfig


headersAttr : List (Attribute msg)
headersAttr =
    [ Element.spacing 8
    , Element.width fill
    , Element.paddingEach { bottom = 7, top = 0, left = 0, right = 0 }
    , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
    , Border.color Palette.gray.lightest
    ]
