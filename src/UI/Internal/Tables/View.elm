module UI.Internal.Tables.View exposing
    ( cellContentRender
    , cellSpace
    , headersAttr
    , rowBox
    , rowRender
    , simpleHeaderRender
    , topCellSpace
    )

import Element exposing (Attribute, Element, fill, fillPortion, minimum, px, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Keyed as Keyed
import UI.Button as Button
import UI.Internal.Colors as Colors
import UI.Internal.NArray as NArray
import UI.Internal.Primitives as Primitives
import UI.Internal.Tables.Common exposing (..)
import UI.Internal.Utils.Element as InternalElement
import UI.Link as Link exposing (Link)
import UI.Palette as Palette exposing (brightnessMiddle, toneGray, tonePrimary)
import UI.RenderConfig exposing (RenderConfig)
import UI.Tables.Common as Common exposing (..)
import UI.Text as Text exposing (Text, ellipsizeWithTooltip)


cellContentRender : RenderConfig -> Common.Cell msg -> Element msg
cellContentRender renderConfig cell_ =
    case cell_ of
        CellText text ->
            simpleText renderConfig text

        CellButton button ->
            Button.renderElement renderConfig button

        CellLink link text ->
            linkedText renderConfig link text

        CellCustom element ->
            element


simpleText : RenderConfig -> Text -> Element msg
simpleText renderConfig text =
    text
        |> Text.withOverflow ellipsizeWithTooltip
        |> Text.renderElement renderConfig
        |> Element.el cellTextAttributes
        |> Element.el
            [ Element.width fill
            , InternalElement.overflowVisible
            ]


linkedText : RenderConfig -> Link -> Text -> Element msg
linkedText renderConfig link text =
    text
        |> Text.withOverflow ellipsizeWithTooltip
        |> Text.withColor (Palette.color tonePrimary brightnessMiddle)
        |> Text.renderElement renderConfig
        |> Element.el cellTextAttributes
        |> Link.wrapElement renderConfig
            [ Element.width fill
            , InternalElement.overflowVisible
            , Palette.color tonePrimary brightnessMiddle
                |> Palette.toElementColor
                |> Font.color

            -- Color twice, otherwise underline is black
            , Font.underline
            ]
            link


cellTextAttributes : List (Attribute msg)
cellTextAttributes =
    [ Element.width fill
    , Element.clipX
    , Element.paddingXY 8 0
    , Element.centerY
    , InternalElement.overflowVisible
    ]


widthToEl : Common.ColumnWidth -> Element.Length
widthToEl width =
    case width of
        WidthPortion int ->
            fillPortion int |> minimum 0

        WidthPixels int ->
            px int


rowRender : RenderConfig -> ToRow msg item columns -> List Column -> item -> ( String, List ( String, Element msg ) )
rowRender renderConfig toRow columns item =
    ( toRow.toKey item
    , toRow.view item
        |> NArray.toList
        |> List.map2 (cellRender renderConfig) columns
    )


rowBox : ( String, List ( String, Element msg ) ) -> ( String, Element msg )
rowBox ( key, cells ) =
    ( "#" ++ key
    , Keyed.row
        [ Element.spacing 8
        , Primitives.defaultRoundedBorders
        , Element.width fill
        , Element.mouseOver [ Background.color Colors.gray.light3 ]
        ]
        cells
    )


cellRender : RenderConfig -> Column -> Common.Cell msg -> ( String, Element msg )
cellRender renderConfig (Column label { width }) cell =
    ( "#" ++ label
    , cell
        |> cellContentRender renderConfig
        |> cellSpace width
    )


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
        |> cellContentRender renderConfig


headersAttr : List (Attribute msg)
headersAttr =
    [ Element.spacing 8
    , Element.width fill
    , Element.paddingEach { bottom = 7, top = 0, left = 0, right = 0 }
    , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
    , Border.color Colors.gray.light3
    ]
