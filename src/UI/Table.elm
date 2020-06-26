module UI.Table exposing
    ( Table, table
    , HeaderRow, headersEnd, header
    , Row, withStaticRows, rowEnd, cellFromText, cellFromButton
    , OptRow
    , CellWidth, withWidth, withCellsWidth, cellWidthEnd, cellWidthPixels, cellWidthPortion
    , withResponsiveRows, ResponsiveConfig, MobileCover
    , withCellsDetails, cellMobileDetailsEnd, cellMobileDetailsHide, cellMobileDetailsShow, cellMobileDetailsShowIf
    , renderElement
    )

{-| Tables are a matrixial data disposition with rows, columns, headers, and cells.

`UI.Tables` are type-safe, which means that every row needs to have the same number of columns (including the headers). Otherwise, compilation fails.

    Table.table
        (header "Name" <| header "Age" <| headersEnd)
        |> Table.withStaticRows
            [ rowEnd
                |> cellFromText (Text.body1 "24")
                |> cellFromText (Text.body1 "Catarina")
            , rowEnd
                |> cellFromText (Text.body1 "30")
                |> cellFromText (Text.body1 "Gabriel")
            ]
        |> Table.renderElement renderConfig


# Building

@docs Table, table


# Header

@docs HeaderRow, headersEnd, header


# Rows

@docs Row, withStaticRows, rowEnd, cellFromText, cellFromButton


# Columns parameteres

@docs OptRow


## Cell's width

@docs CellWidth, withWidth, withCellsWidth, cellWidthEnd, cellWidthPixels, cellWidthPortion


# Mobile

@docs withResponsiveRows, ResponsiveConfig, MobileCover


## Cell's details

@docs withCellsDetails, cellMobileDetailsEnd, cellMobileDetailsHide, cellMobileDetailsShow, cellMobileDetailsShowIf


# Rendering

@docs renderElement

-}

import Element exposing (Element, fill, fillPortion, px, shrink)
import Element.Border as Border
import UI.Button as Button exposing (Button)
import UI.Internal.Basics exposing (..)
import UI.Internal.NList as NList exposing (NList)
import UI.Internal.Palette as Palette
import UI.Internal.TypeNumbers as T
import UI.Palette as Palette exposing (brightnessMiddle, tonePrimary)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.RowList as RowList
import UI.Text as Text exposing (Text)
import UI.Utils.Element exposing (zeroPadding)


{-| The `Table msg object columns` type is used for describing the component for later rendering.
-}
type Table msg object columns
    = Table (Properties columns) (Options msg object columns)


type alias Properties columns =
    { headers : HeaderRow columns
    }


type alias Options msg object columns =
    { rows : Rows msg object columns
    , width : Element.Length
    }


{-| Upholds instructions on how to render the rows as both tables on desktop and toggleable-lists on mobile.

    { detailsShowLabel = "Show details"
    , detailsCollapseLabel = "Hide details"
    , toRow = desktopRowMap -- (object -> Row msg)
    , toCover = mobileCover -- (object -> MobileCover)
    , selectMsg = Msg.SelectSomeone
    , isSelected = isSelected
    , items = model.items
    }

-}
type alias ResponsiveConfig msg object columns =
    { detailsShowLabel : String
    , detailsCollapseLabel : String
    , toRow : object -> Row msg columns
    , toCover : object -> MobileCover
    , selectMsg : object -> msg
    , isSelected : object -> Bool
    , items : List object
    }


type Rows msg object columns
    = StaticRows (List (Row msg columns))
    | ResponsiveRows (ResponsiveConfig msg object columns)


{-| Alias to [`UI.RowList.ToggleableCover`](UI-RowList#ToggleableCover).
-}
type alias MobileCover =
    RowList.ToggleableCover


{-| `Row msg columns` upholds every cell from a table's row.
This is a type-safe sized-array.

Use [`Table.rowEnd`](UI-Table#rowEnd) and [`Table.cellFromText`](UI-table#cellFromText) or [`Table.cellFromButton`](UI-table#cellFromButton) to build it.

-}
type alias Row msg columns =
    NList (Cell msg) columns


{-| `HeaderRow columns` upholds every label from a table's header.
This is a type-safe sized-array.

Use [`Table.headersEnd`](UI-Table#headersEnd) and [`Table.header`](UI-table#header) to build it.

-}
type alias HeaderRow columns =
    NList HeaderCell columns


{-| `OptRow value columns` upholds option-applying rows.
E.g., `value` is replaced with [`CellWidth`](UI-Table#CellWidth) for [`Table.withCellsWidth`](UI-Table#withCellsWidth).
-}
type alias OptRow value columns =
    NList (Maybe value) columns


type alias HeaderCell =
    { title : String
    , width : CellWidth
    , detailsVisible : Bool
    }


type Cell msg
    = CellText Text
    | CellButton (Button msg)


{-| `CellWidth` specifies a cell's width.
-}
type CellWidth
    = WidthPixels Int
    | WidthPortion Int


{-| Marks the end of a [`HeaderRow`](UI-Table#HeaderRow).

    Table.header "Barcode" <| Table.header "Brand" <| Table.header "Manage" <| Table.headersEnd

-}
headersEnd : HeaderRow T.Zero
headersEnd =
    NList.empty


{-| Marks the end of a [`Row msg`](UI-Table#Row).

    Table.rowEnd
        |> Table.cellFromButton -- THIRD CELL
            (Button.cmd (Msg.Delete "123-456") (Button.fromLabel "Delete"))
        |> Table.cellFromText (Text.caption "Nestlé") -- SECOND CELL
        |> Table.cellFromText (Text.body1 "123-456") -- FIRST CELL

-}
rowEnd : Row msg T.Zero
rowEnd =
    NList.empty


{-| Prepend a columns's header to a [`HeaderRow`](UI-Table#HeaderRow).

    Table.header "Name" <| Table.header "Age" <| Table.header "Country" <| Table.headersEnd

-}
header : String -> HeaderRow columns -> HeaderRow (T.Increase columns)
header head tail =
    NList.cons { title = head, width = WidthPortion 1, detailsVisible = True } tail


{-| Transforms a `UI.Text` into a cell prepending it to a row.

    Table.rowEnd
        |> Table.cellFromText (Text.caption "Morning") -- SECOND CELL
        |> Table.cellFromText (Text.body1 "Benzetacil") -- FIRST CELL

-}
cellFromText : Text -> Row msg columns -> Row msg (T.Increase columns)
cellFromText text tail =
    NList.cons (CellText text) tail


{-| Transforms a `UI.Button` into a cell prepending it to a row.

    Table.rowEnd
        |> Table.cellFromButton
            -- THIRD CELL
            (Button.cmd (Msg.CandidateSetApproved 1 False) (Button.fromLabel "Deny"))
        |> Table.cellFromButton
            -- SECOND CELL
            (Button.cmd (Msg.CandidateSetApproved 1 True) (Button.fromLabel "Approve"))
        |> Table.cellFromText
            -- FIRST CELLL
            (Text.body1 "Candidate 01")

-}
cellFromButton : Button msg -> Row msg columns -> Row msg (T.Increase columns)
cellFromButton btn tail =
    NList.cons (CellButton btn) tail


{-| Even if there is no data, all tables must contain headers.
So, here the building of tables start.

    Table.table (header "Name" <| header "Age" <| headersEnd)

-}
table : HeaderRow columns -> Table msg object columns
table headers =
    Table { headers = headers } defaultOptions



-- Options


{-| `Table.withStaticRows` is the simplest way to add rows to a `Table`.
However, it does not produce a mobile-compatible component.
Thus, not recommended for Tables with more than two columns or huge cells.

    Table.withStaticRows
        [ rowEnd
            -- SECOND CELL
            |> cellFromText (Text.body1 "24")
            -- FIRST CELL
            |> cellFromText (Text.body1 "Catarina")
        , rowEnd
            |> cellFromText (Text.body1 "30")
            |> cellFromText (Text.body1 "Gabriel")
        ]
        someTable

-}
withStaticRows : List (Row msg columns) -> Table msg object columns -> Table msg object columns
withStaticRows rows (Table prop opt_) =
    Table prop { opt_ | rows = StaticRows rows }


{-| `Table.withResponsiveRows` insert rows to a table and describes a ToggableList for pages rendering on a mobile's screen.

    Table.withResponsiveRows
        { detailsShowLabel = "Show details"
        , detailsCollapseLabel = "Hide details"
        , toRow =
            \{ name, age } ->
                rowEnd
                    |> cellFromText (Text.caption age)
                    |> cellFromText (Text.body1 name)
        , toCover =
            \{ name, age } -> { title = name, caption = Just age }
        , selectMsg = Msg.SelectSomeone
        , isSelected = isSelected
        , items = [ { name = "John", age = "29yo" }, { name = "Maria", age = "24yo" } ]
        }
        someTable

-}
withResponsiveRows : ResponsiveConfig msg object columns -> Table msg object columns -> Table msg object columns
withResponsiveRows responsiveOpt (Table prop opt_) =
    Table prop { opt_ | rows = ResponsiveRows responsiveOpt }


{-| Applies [`Element.width`](/packages/mdgriffith/elm-ui/latest/Element#width) to the component.

    Table.withWidth
        (Element.fill |> Element.minimum 220)
        someTable

-}
withWidth : Element.Length -> Table msg object columns -> Table msg object columns
withWidth width (Table prop opt_) =
    Table prop { opt_ | width = width }


{-| Applies a different width for each column in the table.

    Table.withCellsWidth
        (cellWidthPortion 4 <| cellWidthPortion 5 <| cellWidthEnd)
        someTwoColumnsTable

-}
withCellsWidth : OptRow CellWidth columns -> Table msg object columns -> Table msg object columns
withCellsWidth row (Table prop opt_) =
    let
        mergeWidth oldHeader maybeWidth =
            case maybeWidth of
                Just len ->
                    { oldHeader | width = len }

                Nothing ->
                    oldHeader
    in
    Table { prop | headers = NList.map2 mergeWidth prop.headers row } opt_


{-| Selects which columns should show or hide when toggling the mobile's details.

    Table.withCellsDetails
        (cellMobileDetailsEnd
            |> cellMobileDetailsShow
            -- THIRD COLUMN
            |> cellMobileDetailsShow
            -- SECOND COLUMN
            |> cellMobileDetailsHide
         -- FIRST COLUMN
        )
        someThreeColumnsTable

-}
withCellsDetails : OptRow Bool columns -> Table msg object columns -> Table msg object columns
withCellsDetails row (Table prop opt_) =
    let
        mergeVisibility oldHeader maybeVisibility =
            case maybeVisibility of
                Just visibility ->
                    { oldHeader | detailsVisible = visibility }

                Nothing ->
                    oldHeader
    in
    Table { prop | headers = NList.map2 mergeVisibility prop.headers row } opt_


{-| Similar to [`Element.fillPortion`](/packages/mdgriffith/elm-ui/latest/Element#fillPortion) but applied to an entire Table's column.

    cellWidthEnd
        |> cellWidthPortion 20 -- THIRD COLUMN
        |> cellWidthPortion 31 -- SECOND COLUMN
        |> cellWidthPortion 17 -- FIRST COLUMN

-}
cellWidthPortion : Int -> OptRow CellWidth columns -> OptRow CellWidth (T.Increase columns)
cellWidthPortion int accu =
    opt (WidthPortion int) accu


{-| Similar to [`Element.px`](/packages/mdgriffith/elm-ui/latest/Element#px) but applied to an entire Table's column.

    cellWidthEnd
        |> cellWidthPixels 320 -- THIRD COLUMN
        |> cellWidthPixels 240 -- SECOND COLUMN
        |> cellWidthPixels 480 -- FIRST COLUMN

-}
cellWidthPixels : Int -> OptRow CellWidth columns -> OptRow CellWidth (T.Increase columns)
cellWidthPixels int accu =
    opt (WidthPixels int) accu


{-| Marks the end of a [`OptRow CellWidth`](UI-Table#CellWidth).

    cellWidthPortion 3 <| cellWidthPixels 240 <| cellWidthPortion 2 <| cellWidthEnd

-}
cellWidthEnd : OptRow CellWidth T.Zero
cellWidthEnd =
    optsEnd


{-| Marks the end of a [`OptRow CellWidth`](UI-Table#CellWidth).

    cellWidthEnd
        |> cellWidthPortion 2 -- THIRD COLUMN
        |> cellWidthPixels 240 -- SECOND COLUMN
        |> cellWidthPortion 3 -- FIRST COLUMN

-}
cellMobileDetailsEnd : OptRow Bool T.Zero
cellMobileDetailsEnd =
    optsEnd


{-| Defines that an entire column is to show in the mobile's details of the selected row.

    cellMobileDetailsEnd
        |> cellMobileDetailsShow -- SHOW THE THIRD COLUMN
        |> cellMobileDetailsHide -- HIDE THE SECOND COLUMN
        |> cellMobileDetailsHide -- HIDE THE FIRST COLUMN

-}
cellMobileDetailsShow : OptRow Bool columns -> OptRow Bool (T.Increase columns)
cellMobileDetailsShow accu =
    opt True accu


{-| Defines that an entire column won't appear in the mobile's details of the selected row.

    cellMobileDetailsEnd
        |> cellMobileDetailsShow -- SHOW THE THIRD COLUMN
        |> cellMobileDetailsHide -- HIDE THE SECOND COLUMN
        |> cellMobileDetailsHide -- HIDE THE FIRST COLUMN

-}
cellMobileDetailsHide : OptRow Bool columns -> OptRow Bool (T.Increase columns)
cellMobileDetailsHide accu =
    opt False accu


{-| Given a condition, defines that an entire column appears or not in the mobile's details of the selected row.

    cellMobileDetailsEnd
        |> cellMobileDetailsShow -- SHOW THE THIRD COLUMN
        |> cellMobileDetailsShowIf (isRainingInNewYork model) -- MAYBE HIDE THE SECOND COLUMN
        |> cellMobileDetailsHide -- HIDE THE FIRST COLUMN

-}
cellMobileDetailsShowIf : Bool -> OptRow Bool columns -> OptRow Bool (T.Increase columns)
cellMobileDetailsShowIf condition accu =
    opt condition accu



-- Render


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Table msg object columns -> Element msg
renderElement cfg (Table { headers } { rows, width }) =
    case rows of
        ResponsiveRows responsiveOpt ->
            if RenderConfig.isMobile cfg then
                mobileView cfg headers responsiveOpt

            else
                responsiveOpt.items
                    |> List.map responsiveOpt.toRow
                    |> desktopView cfg True headers width

        StaticRows desktopRows ->
            desktopView cfg False headers width desktopRows



-- Internals


desktopView : RenderConfig -> Bool -> HeaderRow columns -> Element.Length -> List (Row msg columns) -> Element msg
desktopView cfg responsive headers width desktopRows =
    let
        rowRender row =
            row
                |> NList.map2 (cellRender cfg) headers
                |> NList.toList
                |> Element.row [ Element.spacing 4, Element.width fill ]
    in
    desktopRows
        |> List.map rowRender
        |> (::)
            (headers
                |> NList.map (headerRender cfg)
                |> NList.toList
                |> Element.row
                    [ Element.spacing 4
                    , Element.width fill
                    , Element.paddingEach { bottom = 9, top = 0, left = 0, right = 0 }
                    , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                    , Border.color Palette.gray.lightest
                    ]
            )
        |> Element.column
            [ Element.spacing 16
            , Element.width width
            , Element.paddingEach <|
                ifThenElse responsive
                    { top = 20, left = 20, right = 20, bottom = 0 }
                    zeroPadding
            ]


mobileView : RenderConfig -> HeaderRow columns -> ResponsiveConfig msg object columns -> Element msg
mobileView renderConfig headers responsiveOpt =
    let
        rowMap object =
            object
                |> responsiveOpt.toRow
                |> NList.map2 Tuple.pair headers
                |> NList.toList

        detailApplier ( cellHeader, cell ) =
            if cellHeader.detailsVisible then
                Just
                    ( cellHeader.title
                    , cellRender renderConfig cellHeader cell
                    )

            else
                Nothing

        details object =
            object
                |> rowMap
                |> List.filterMap detailApplier
    in
    RowList.toggleableList
        { detailsShowLabel = responsiveOpt.detailsShowLabel
        , detailsCollapseLabel = responsiveOpt.detailsCollapseLabel
        , toCover = responsiveOpt.toCover
        , toDetails = details
        , selectMsg = responsiveOpt.selectMsg
        }
        |> RowList.withOptions responsiveOpt.items
        |> RowList.withSelected responsiveOpt.isSelected
        |> RowList.renderElement renderConfig


headerRender : RenderConfig -> HeaderCell -> Element msg
headerRender cfg { title, width } =
    Text.overline title
        |> Text.withColor (Palette.color tonePrimary brightnessMiddle)
        |> Text.renderElement cfg
        |> Element.el [ Element.width (widthToEl width) ]


detailRender : RenderConfig -> Cell msg -> Element msg
detailRender cfg cell_ =
    case cell_ of
        CellText text ->
            text
                |> Text.renderElement cfg

        CellButton button ->
            Button.renderElement cfg button


cellRender : RenderConfig -> HeaderCell -> Cell msg -> Element msg
cellRender cfg { width } cell_ =
    cell_
        |> detailRender cfg
        |> Element.el [ Element.width (widthToEl width) ]


widthToEl : CellWidth -> Element.Length
widthToEl width =
    case width of
        WidthPortion int ->
            fillPortion int

        WidthPixels int ->
            px int


defaultOptions : Options msg object columns
defaultOptions =
    { rows = StaticRows []
    , width = shrink
    }


optsEnd : OptRow value T.Zero
optsEnd =
    NList.empty


opt : value -> OptRow value columns -> OptRow value (T.Increase columns)
opt head tail =
    NList.cons (Just head) tail
