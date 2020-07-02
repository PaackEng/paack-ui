module UI.Table exposing
    ( Table, State, Msg, Cell, table, withItems
    , Columns, Column, columnsEmpty, columnsPushHeader, columnsPush
    , Row, rowEmpty, rowPushText, rowPushButton
    , Responsive, Cover, Details, Detail, withResponsive
    , withFilters
    , withWidth
    , renderElement
    , rowPush
    )

{-| Tables are a matrixial data disposition with rows, columns, headers, and cells.

`UI.Tables` are type-safe, which means that every row needs to have the same number of columns (including the headers). Otherwise, compilation fails.

    Table.table Msg.ForTable
        Book.tableColumns
        Book.toTableRow
        |> Table.withResponsive
            { toDetails = Book.toTableDetails
            , toCover = Book.toTableCover
            }
        |> Table.withState model.tableState
        |> Table.withWidth Table.widthFull
        |> Table.withFilters someFilters
        |> Table.withItems
            [ Book "Dan Brown" "The Da Vinci Code" ]
        |> Table.renderElement cfg


# Table

@docs Table, State, Msg, Cell, table, withItems


## Desktop

@docs Columns, Column, columnsEmpty, columnsPushHeader, columnsPush


## Desktop Rows

@docs Row, rowEmpty, rowPushText, rowPushButton


## Mobile

@docs Responsive, Cover, Details, Detail, withResponsive


# Filters

@docs withFilters


# Width

@docs withWidth


# Rendering

@docs renderElement

-}

import Element exposing (Attribute, Element, fill, fillPortion, minimum, px, shrink)
import Internal.Basics exposing (swap)
import Internal.NArray as NArray exposing (NArray)
import UI.Button as Button exposing (Button)
import UI.Internal.Fitlers as Filters exposing (..)
import UI.Text as Text exposing (Text)


{-| The `Table msg item columns` type is used for describing the component for later rendering.
-}
type Table msg item columns
    = Table (Properties msg item columns) (Options msg item columns)


type alias Properties msg item columns =
    { columns : Columns columns
    , toRow : ToRow msg item columns
    , toExtern : Msg -> msg
    }


type alias Options msg item columns =
    { items : List item
    , filters : Maybe (Filters msg item columns)
    , width : Element.Length
    , state : Maybe State
    }


type Msg
    = Msg


type State
    = State StateModel


type alias StateModel =
    { filters : Dict Int FilterModel
    }


type alias ToRow msg item columns =
    item -> Row msg columns


table : (Msg -> msg) -> Columns columns -> ToRow msg item columns -> Table msg item columns
table toExtern columns toRow =
    Table { columns = columns, toRow = toRow, toExtern = toExtern } defaultOptions


defaultOptions : Options msg item columns
defaultOptions =
    { items = []
    , filters = Nothing
    , width = shrink
    , state = Nothing
    }


withItems : List item -> Table msg item columns -> Table msg item columns
withItems items (Table prop opt) =
    Table prop { opt | items = items }



-- Columns


type alias Columns columns =
    NArray Column columns


type Column
    = Column String ColumnOptions


type alias ColumnOptions =
    { header : String
    , width : ColumnWidth
    }


{-| `ColumnWidth` specifies a cell's width.
-}
type ColumnWidth
    = WidthPixels Int
    | WidthPortion Int


columnsEmpty : Columns T.Zero
columnsEmpty =
    NArray.empty


columnsPushHeader : String -> Columns columns -> Columns (T.Increase columns)
columnsPushHeader header accu =
    NArray.append (headerToColumn header) accu


columnsPush : Column -> Columns columns -> Columns (T.Increase columns)
columnsPush header config accu =
    NArray.append column accu


defaultColumnWidth : ColumnWidth
defaultColumnWidth =
    WidthPortion 1


headerToColumn : String -> Column
headerToColumn header =
    Column header defaultColumnConfig


{-| Similar to [`Element.fillPortion`](/packages/mdgriffith/elm-ui/latest/Element#fillPortion) but applied to an entire Table's column.

    columnEmpty
        |> columnsPush (headerToColumn "Title" |> columnWidthPortion 3)
        |> columnsPush (headerToColumn "Author" |> columnWidthPortion 3)
        |> columnsPush (headerToColumn "Year" |> columnWidthPortion 2)

-}
columnWidthPortion : Int -> Column -> Column
columnWidthPortion int (Column header opt) =
    Column header { opt | width = WidthPortion value }


{-| Similar to [`Element.px`](/packages/mdgriffith/elm-ui/latest/Element#px) but applied to an entire Table's column.

    columnEmpty
        |> columnsPush (headerToColumn "Title" |> columnWidthPixels 320)
        |> columnsPush (headerToColumn "Author" |> columnWidthPixels 320)
        |> columnsPush (headerToColumn "Year" |> columnWidthPixels 240)

-}
columnWidthPixels : Int -> Column -> Column
columnWidthPixels value (Column header opt) =
    Column header { opt | width = WidthPixels value }



-- Desktop Rows


type alias Row msg columns =
    NArray (Cell msg) columns


type Cell msg
    = CellText Text
    | CellButton (Button msg)


rowEmpty : Row msg T.Zero
rowEmpty =
    NArray.empty


{-| Transforms a `UI.Text` into a cell appending it to a row.

TODO: Example

-}
rowPushText : Text -> Row msg columns -> Row msg (T.Increase columns)
rowPushText text accu =
    NArray.append (CellText text) accu


{-| Transforms a `UI.Button` into a cell appending it to a row.

TODO: Example

-}
rowPushButton : Button msg -> Row msg columns -> Row msg (T.Increase columns)
rowPushButton btn accu =
    NArray.append (CellButton btn) accu



-- Responsive


type alias Cover =
    ListView.ToggleableCover


type alias Detail msg =
    { label : String, content : Cell msg }


type alias Details msg columns =
    NArray (Maybe (Detail msg)) columns
