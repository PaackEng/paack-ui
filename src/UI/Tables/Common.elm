module UI.Tables.Common exposing
    ( Columns, columnsEmpty, column
    , ColumnWidth, columnWidthPortion, columnWidthPixels
    , Row, ToRow, rowEmpty, rowCellText, rowCellButton
    , Cell, cellFromText, cellFromButton
    )

{-|


## Desktop

@docs Columns, Column, columnsEmpty, column


## Individual column

@docs ColumnWidth, columnWidthPortion, columnWidthPixels


## Desktop rows

@docs Row, ToRow, rowEmpty, rowCellText, rowCellButton


# Individual cell

@docs Cell, cellFromText, cellFromButton

-}

import UI.Button exposing (Button)
import UI.Internal.NArray as NArray exposing (NArray)
import UI.Internal.Table as Internal exposing (..)
import UI.Internal.TypeNumbers as T
import UI.Text exposing (Text)



-- Columns


type alias Columns columns =
    NArray Column columns


{-| `ColumnWidth` specifies a cell's width.
-}
type alias ColumnWidth =
    Internal.ColumnWidth


columnsEmpty : Columns T.Zero
columnsEmpty =
    NArray.empty


column : String -> ColumnWidth -> Columns columns -> Columns (T.Increase columns)
column header width accu =
    NArray.push (Column header { width = width }) accu


{-| Similar to [`Element.fillPortion`](/packages/mdgriffith/elm-ui/latest/Element#fillPortion) but applied to an entire Table's column.

    columnEmpty
        |> column "Title" (columnWidthPortion 3)
        |> column "Author" (columnWidthPortion 3)
        |> column "Year" (columnWidthPortion 2)

-}
columnWidthPortion : Int -> ColumnWidth
columnWidthPortion value =
    WidthPortion value


{-| Similar to [`Element.px`](/packages/mdgriffith/elm-ui/latest/Element#px) but applied to an entire Table's column.

    columnEmpty
        |> column "Title" (columnWidthPixels 320)
        |> column "Author" (columnWidthPixels 320)
        |> column "Year" (columnWidthPixels 240)

-}
columnWidthPixels : Int -> ColumnWidth
columnWidthPixels value =
    WidthPixels value



-- Cells


type alias Cell msg =
    Internal.Cell msg


cellFromText : Text -> Cell msg
cellFromText text =
    CellText text


cellFromButton : Button msg -> Cell msg
cellFromButton text =
    CellButton text



-- Desktop Rows


type alias Row msg columns =
    NArray (Cell msg) columns


type alias ToRow msg item columns =
    item -> Row msg columns


rowEmpty : Row msg T.Zero
rowEmpty =
    NArray.empty


{-| Transforms a `UI.Text` into a cell appending it to a row.

TODO: Example

-}
rowCellText : Text -> Row msg columns -> Row msg (T.Increase columns)
rowCellText text accu =
    NArray.push (CellText text) accu


{-| Transforms a `UI.Button` into a cell appending it to a row.

TODO: Example

-}
rowCellButton : Button msg -> Row msg columns -> Row msg (T.Increase columns)
rowCellButton btn accu =
    NArray.push (CellButton btn) accu
