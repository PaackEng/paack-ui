module UI.Tables.Stateless exposing
    ( StatelessTable, StatelessConfig, table, withItems
    , withWidth
    , renderElement
    )

{-| Tables are a matrixial data disposition with rows, columns, headers, and cells.

`UI.Tables` are type-safe, which means that every row needs to have the same number of columns (including the headers). Otherwise, compilation fails.

    Stateless.table
        { columns = Book.tableColumns
        , toRow = Book.toTableRow
        }
        |> Stateless.withWidth (Element.fill |> Element.maximum 640)
        |> Stateless.withItems
            [ Book "Dan Brown" "Angels & Demons" "2000"
            , Book "Dan Brown" "The Da Vinci Code" "2003"
            , Book "Dan Brown" "The Lost Symbol" "2009"
            , Book "Dan Brown" "Inferno" "2013"
            , Book "Dan Brown" "Origin" "2017"
            ]
        |> Stateless.renderElement renderConfig

Where `Book` is:

    type alias Book =
        { author : String, title : String, year : String }

    tableColumns =
        columnsEmpty
            |> column "Title" (columnWidthPixels 320)
            |> column "Author" (columnWidthPixels 240)
            |> column "Year" (columnWidthPixels 120)

    toTableRow { author, title, year } =
        rowEmpty
            |> rowCellText (Text.body1 title)
            |> rowCellText (Text.body2 author)
            |> rowCellText (Text.caption year)


# Stateless

@docs StatelessTable, StatelessConfig, table, withItems


# Width

@docs withWidth


# Rendering

@docs renderElement

-}

import Element exposing (Element, shrink)
import UI.Internal.NArray as NArray
import UI.Internal.Tables.Common exposing (..)
import UI.Internal.Tables.View exposing (..)
import UI.RenderConfig exposing (RenderConfig)
import UI.Tables.Common exposing (..)


{-| The `StatelessTable msg item columns` type is used for describing the component for later rendering.

This is type that constrains type-safe sized-arrays.
See [`TypeNumbers`](UI-Utils-TypeNumbers) for how to compose its phantom type.

-}
type StatelessTable msg item columns
    = Table (StatelessConfig msg item columns) (Options item)


{-| Record with parameters for the creation of a [`StatelessTable`](#table).

This is record that constrains type-safe sized-arrays.
See [`TypeNumbers`](UI-Utils-TypeNumbers) for how to compose its phantom type.

-}
type alias StatelessConfig msg item columns =
    { columns : Columns columns
    , toRow : ToRow msg item columns
    }


type alias Options item =
    { items : List item
    , width : Element.Length
    }


{-| Constructs a stateless table from its columns and rows.

    table
        { columns = Book.tableColumns
        , toRow = Book.toTableRow
        }

-}
table : StatelessConfig msg item columns -> StatelessTable msg item columns
table config =
    Table config defaultOptions


defaultOptions : Options item
defaultOptions =
    { items = []
    , width = shrink
    }


{-| Each of these items will become a row in this table.

    withItems
        [ Book "Dan Brown" "Angels & Demons" "2000"
        , Book "Dan Brown" "The Da Vinci Code" "2003"
        , Book "Dan Brown" "The Lost Symbol" "2009"
        , Book "Dan Brown" "Inferno" "2013"
        , Book "Dan Brown" "Origin" "2017"
        ]
        someTable

-}
withItems : List item -> StatelessTable msg item columns -> StatelessTable msg item columns
withItems items (Table prop opt) =
    Table prop { opt | items = items }



-- Width


{-| Applies [`Element.width`](/packages/mdgriffith/elm-ui/latest/Element#width) to the component.

    Table.withWidth
        (Element.fill |> Element.minimum 220)
        someTable

-}
withWidth : Element.Length -> StatelessTable msg item columns -> StatelessTable msg item columns
withWidth width (Table prop opt_) =
    Table prop { opt_ | width = width }



-- Rendering


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> StatelessTable msg item columns -> Element msg
renderElement renderConfig (Table prop opt) =
    desktopView renderConfig prop opt



-- Dekstop rendering


desktopView :
    RenderConfig
    -> StatelessConfig msg item columns
    -> Options item
    -> Element msg
desktopView renderConfig prop opt =
    let
        columns =
            NArray.toList prop.columns

        headers =
            headersRender renderConfig columns

        rows =
            List.map (rowRender renderConfig prop.toRow Nothing columns >> rowBox) opt.items
    in
    Element.column
        [ Element.spacing 2
        , Element.width opt.width
        ]
        (headers :: rows)


headersRender :
    RenderConfig
    -> List Column
    -> Element msg
headersRender renderConfig columns =
    Element.row
        headersAttr
        (List.map (headerRender renderConfig) columns)



-- Headers


headerRender :
    RenderConfig
    -> Column
    -> Element msg
headerRender renderConfig (Column header { width }) =
    header
        |> simpleHeaderRender renderConfig
        |> cellSpace width
