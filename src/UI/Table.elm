module UI.Table exposing
    ( Table, State, Msg, Cell, table, stateInit, withItems
    , Columns, Column, columnsEmpty, columnsPushHeader, columnsPush
    , Row, rowEmpty, rowPushText, rowPushButton
    , Responsive, Cover, Details, Detail, withResponsive, detailsEmpty, detailsPush, detailsPushHidden
    , Filters, withFilters, filtersEmpty, filtersPushSingleText
    , Strategy, filterLocal, filterRemote
    , withWidth
    , renderElement
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

Where:

    tableColumns =
        columnsEmpty
            |> columnsPushHeader "Title"
            |> columnsPush (headerToColumn "Author" |> columnWidthPortion 3)

    toTableRow { author, title } =
        rowEmpty
            |> rowPushText (Text.body1 title)
            |> rowPushText (Text.body2 author)

    toTableDetails { author, title } =
        detailsEmpty
            |> detailsPushHidden
            |> detailsPush { label = "Author", content = cellFromText <| Text.body2 author }

    toTableCover { author, title } =
        { title = title, caption = Nothing }

    someFilters =
        fitersEmpty
            |> filtersPushSingleText "" (filterLocal (\{ title } str -> String.contains str title))
            |> filtersPushSingleText "" (filterRemote { editMsg = Msg.FilterAuthor })


# Table

@docs Table, State, Msg, Cell, table, stateInit, withItems


## Desktop

@docs Columns, Column, columnsEmpty, columnsPushHeader, columnsPush


## Desktop Rows

@docs Row, rowEmpty, rowPushText, rowPushButton


## Mobile

@docs Responsive, Cover, Details, Detail, withResponsive, detailsEmpty, detailsPush, detailsPushHidden


# Filters

@docs Filters, withFilters, filtersEmpty, filtersPushSingleText


## Filter's Strategy

@docs Strategy, filterLocal, filterRemote


# Width

@docs withWidth


# Rendering

@docs renderElement

-}

import Dict exposing (Dict)
import Element exposing (Attribute, Element, fill, fillPortion, minimum, px, shrink)
import UI.Button as Button exposing (Button)
import UI.Internal.Basics exposing (swap)
import UI.Internal.Filters as Filters
import UI.Internal.NArray as NArray exposing (NArray)
import UI.Internal.TypeNumbers as T
import UI.ListView as ListView
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
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
    , responsive : Maybe (Responsive msg item columns)
    }


type Msg
    = Msg


type State
    = State StateModel


type alias StateModel =
    { filters : Dict Int Filters.FilterModel
    , mobileSelected : Maybe Int
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
    , responsive = Nothing
    }


stateInit : State
stateInit =
    State { filters = Dict.empty, mobileSelected = Nothing }


withItems : List item -> Table msg item columns -> Table msg item columns
withItems items (Table prop opt) =
    Table prop { opt | items = items }



-- Columns


type alias Columns columns =
    NArray Column columns


type Column
    = Column String ColumnOptions


type alias ColumnOptions =
    { width : ColumnWidth
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
    NArray.push (headerToColumn header) accu


columnsPush : Column -> Columns columns -> Columns (T.Increase columns)
columnsPush column accu =
    NArray.push column accu


headerToColumn : String -> Column
headerToColumn header =
    Column header { width = defaultColumnWidth }


defaultColumnWidth : ColumnWidth
defaultColumnWidth =
    WidthPortion 1


{-| Similar to [`Element.fillPortion`](/packages/mdgriffith/elm-ui/latest/Element#fillPortion) but applied to an entire Table's column.

    columnEmpty
        |> columnsPush (headerToColumn "Title" |> columnWidthPortion 3)
        |> columnsPush (headerToColumn "Author" |> columnWidthPortion 3)
        |> columnsPush (headerToColumn "Year" |> columnWidthPortion 2)

-}
columnWidthPortion : Int -> Column -> Column
columnWidthPortion value (Column header opt) =
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
    NArray.push (CellText text) accu


{-| Transforms a `UI.Button` into a cell appending it to a row.

TODO: Example

-}
rowPushButton : Button msg -> Row msg columns -> Row msg (T.Increase columns)
rowPushButton btn accu =
    NArray.push (CellButton btn) accu



-- Cell for third party


cellFromText : Text -> Cell msg
cellFromText text =
    CellText text


cellFromButton : Button msg -> Cell msg
cellFromButton text =
    CellButton text



-- Responsive


type alias Responsive msg item columns =
    { toDetails : item -> Details msg columns
    , toCover : item -> Cover
    }


type alias Cover =
    ListView.ToggleableCover


type alias Detail msg =
    { label : String, content : Cell msg }


type alias Details msg columns =
    NArray (Maybe (Detail msg)) columns


withResponsive : Responsive msg item columns -> Table msg item columns -> Table msg item columns
withResponsive responsive (Table prop opt) =
    Table prop { opt | responsive = Just responsive }


detailsEmpty : Details msg T.Zero
detailsEmpty =
    NArray.empty


detailsPush : Detail msg -> Details msg columns -> Details msg (T.Increase columns)
detailsPush detail accu =
    NArray.push (Just detail) accu


detailsPushHidden : Details msg columns -> Details msg (T.Increase columns)
detailsPushHidden accu =
    NArray.push Nothing accu



-- Filters


type alias Filters msg item columns =
    Filters.Filters msg item columns


type alias Strategy msgs value item =
    Filters.Strategy msgs value item


withFilters : Filters msg item columns -> Table msg item columns -> Table msg item columns
withFilters filters (Table prop opt) =
    Table prop { opt | filters = Just filters }


filtersEmpty : Filters msg item T.Zero
filtersEmpty =
    NArray.empty


filtersPush : Filters.Filter msg item -> Filters msg item columns -> Filters msg item (T.Increase columns)
filtersPush filter accu =
    NArray.push filter accu


filtersPushSingleText :
    String
    -> Filters.SingleTextFilterStrategy msg item
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
filtersPushSingleText init strategy accu =
    { init = init, strategy = strategy }
        |> Filters.SingleTextFilter
        |> swap filtersPush accu


filterLocal : (item -> value -> Bool) -> Strategy msgs value item
filterLocal isKept =
    Filters.Local isKept


filterRemote : msgs -> Strategy msgs value item
filterRemote msgs =
    Filters.Remote msgs



-- Width


withWidth : Element.Length -> Table msg object columns -> Table msg object columns
withWidth width (Table prop opt_) =
    Table prop { opt_ | width = width }



-- Rendering


renderElement : RenderConfig -> Table msg object columns -> Element msg
renderElement renderConfig (Table prop opt) =
    Element.none
