module UI.Tables.Common exposing
    ( Columns, columnsEmpty, column
    , ColumnWidth, columnWidthPortion, columnWidthPixels
    , Row, ToRow, rowEmpty, rowCellText, rowCellButton
    , Cell, cellFromText, cellFromButton
    , cellFromCustom, rowCellCustom
    )

{-|


## Desktop

@docs Columns, columnsEmpty, column


## Individual column

@docs ColumnWidth, columnWidthPortion, columnWidthPixels


## Desktop rows

@docs Row, ToRow, rowEmpty, rowCellText, rowCellButton


# Individual cell

@docs Cell, cellFromText, cellFromButton

-}

import Element exposing (Element)
import UI.Button exposing (Button)
import UI.Internal.NArray as NArray exposing (NArray)
import UI.Internal.Tables.Common as Internal exposing (..)
import UI.Text exposing (Text)
import UI.Utils.TypeNumbers as T



-- Columns


{-| Array with all the columns from a table.

This is a type-safe sized-array.
See [`TypeNumbers`](UI-Utils-TypeNumbers) for how to compose its phantom type.

-}
type alias Columns columns =
    NArray Column columns


{-| `ColumnWidth` specifies a cell's width.
-}
type alias ColumnWidth =
    Internal.ColumnWidth


{-| An empty [`Columns`](#Columns) set.

    columnsEmpty
        |> column "Star" (columnWidthPortion 3)
        |> column "Constellation" (columnWidthPortion 3)
        |> column "Distance" (columnWidthPortion 2)

-}
columnsEmpty : Columns T.Zero
columnsEmpty =
    NArray.empty


{-| Appends a new column to the list of columns, defining its header's label and the entire column's width.

    columnsEmpty
        |> column "Name" (columnWidthPortion 3)
        |> column "Age" (columnWidthPortion 1)

-}
column : String -> ColumnWidth -> Columns columns -> Columns (T.Increase columns)
column header width accu =
    NArray.push (Column header { width = width }) accu


{-| Similar to [`Element.fillPortion`](/packages/mdgriffith/elm-ui/latest/Element#fillPortion) but applied to an entire Table's column.

    columnsEmpty
        |> column "Title" (columnWidthPortion 3)
        |> column "Author" (columnWidthPortion 3)
        |> column "Year" (columnWidthPortion 2)

-}
columnWidthPortion : Int -> ColumnWidth
columnWidthPortion value =
    WidthPortion value


{-| Similar to [`Element.px`](/packages/mdgriffith/elm-ui/latest/Element#px) but applied to an entire Table's column.

    columnEmpty
        |> column "Name" (columnWidthPixels 320)
        |> column "Length" (columnWidthPixels 240)
        |> column "Population" (columnWidthPixels 240)

-}
columnWidthPixels : Int -> ColumnWidth
columnWidthPixels value =
    WidthPixels value



-- Cells


{-| A singular cell of a table.
Can hold texts, buttons or any custom `Element msg`.
-}
type alias Cell msg =
    Internal.Cell msg


{-| Creates a cell with some text content.

    cellFromText <| Text.body2 "Watermelon"

-}
cellFromText : Text -> Cell msg
cellFromText text =
    CellText text


{-| Creates a cell with a button inside.

    Button.fromText "Delete"
        |> Button.cmd Msg.Delete Button.danger
        |> cellFromButton

-}
cellFromButton : Button msg -> Cell msg
cellFromButton text =
    CellButton text


{-| Creates a cell with an `Element msg` inside.

    cellFromCustom <| Element.row [] [ Element.text "Hello", Element.text "World" ]

-}
cellFromCustom : Element msg -> Cell msg
cellFromCustom element =
    Custom element



-- Desktop Rows


{-| Array with all the cells in a single row.

This is a type-safe sized-array.
See [`TypeNumbers`](UI-Utils-TypeNumbers) for how to compose the phantom type.

-}
type alias Row msg columns =
    NArray (Cell msg) columns


{-| Helper for composing a map function for a single row.
-}
type alias ToRow msg item columns =
    item -> Row msg columns


{-| An empty set of cells for a row.

    rowEmpty
        |> rowCellText (Text.body1 "Hello")
        |> rowCellText (Text.body2 "World")

-}
rowEmpty : Row msg T.Zero
rowEmpty =
    NArray.empty


{-| Transforms a `UI.Text` into a cell appending it to a row.

Similar to [`cellFromText`](#cellFromText) but infused for rows.

    rowEmpty
        |> rowCellText (Text.body1 "Coffee")
        |> rowCellText (Text.body2 "Brazil")

-}
rowCellText : Text -> Row msg columns -> Row msg (T.Increase columns)
rowCellText text accu =
    NArray.push (CellText text) accu


{-| Transforms a `UI.Button` into a cell appending it to a row.

Similar to [`cellFromButton`](#cellFromButton) but infused for rows.

    rowEmpty
        |> rowCellText (Text.body1 "Aldebaran")
        |> rowCellButton
            ("See in Stellarium"
                |> Button.fromText
                |> Button.redirect "stellarium://gj/9159"
                    Button.hyperlink
            )

-}
rowCellButton : Button msg -> Row msg columns -> Row msg (T.Increase columns)
rowCellButton btn accu =
    NArray.push (CellButton btn) accu


{-| Transforms any `Element msg` into a cell appending it to a row.

Similar to [`cellFromCustom`](#cellFromCustom) but infused for rows.

    rowEmpty
        |> rowCellText (Text.body1 "Aldebaran")
        |> rowCellCustom (Element.text "Hello")

-}
rowCellCustom : Element msg -> Row msg columns -> Row msg (T.Increase columns)
rowCellCustom element accu =
    NArray.push (Custom element) accu
