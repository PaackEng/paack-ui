module UI.Table exposing
    ( Table, table
    , HeaderRow, headersEnd, header
    , Row, withStaticRows, rowEnd, cellFromText, cellFromButton
    , withResponsiveRows, ResponsiveConfig, MobileCover
    , OptRow
    , ColumnWidth, withColumnsWidth, columnsWidthEnd, columnWidthPixels, columnWidthPortion
    , withColumnsDetails, columnsMobileDetailsEnd, columnMobileDetailsHide, columnMobileDetailsShow, columnMobileDetailsShowIf
    , ColumnFilter, withColumnsFilter, columnsFilterEnd, columnFilterEmpty, columnFiltering, columnFilterEditing
    , FilterEditConfig, FilterField, filterText
    , withWidth
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


## Mobile's Rows

@docs withResponsiveRows, ResponsiveConfig, MobileCover


# Columns' parameters

@docs OptRow


## Columns' width

@docs ColumnWidth, withColumnsWidth, columnsWidthEnd, columnWidthPixels, columnWidthPortion


## Mobile's details

@docs withColumnsDetails, columnsMobileDetailsEnd, columnMobileDetailsHide, columnMobileDetailsShow, columnMobileDetailsShowIf


## Filterable Row

@docs ColumnFilter, withColumnsFilter, columnsFilterEnd, columnFilterEmpty, columnFiltering, columnFilterEditing


### Filter fields

@docs FilterEditConfig, FilterField, filterText


# Table's width

@docs withWidth


# Rendering

@docs renderElement

-}

import Element exposing (Attribute, Element, fill, fillPortion, minimum, px, shrink)
import Element.Border as Border
import Element.Events as Events
import Html.Attributes as HtmlAttrs
import UI.Button as Button exposing (Button)
import UI.Icon as Icon
import UI.Internal.Basics exposing (..)
import UI.Internal.NList as NList exposing (NList)
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Internal.TypeNumbers as T
import UI.ListView as ListView
import UI.Palette as Palette exposing (brightnessMiddle, toneGray)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text exposing (Text)
import UI.TextField as TextField exposing (TextField)
import UI.Utils.Element exposing (zeroPadding)


{-| The `Table msg object columns` type is used for describing the component for later rendering.
-}
type Table msg object columns
    = Table (Properties msg columns) (Options msg object columns)


type alias Properties msg columns =
    { headers : HeaderRow msg columns
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


{-| Alias to [`UI.ListView.ToggleableCover`](UI-ListView#ToggleableCover).
-}
type alias MobileCover =
    ListView.ToggleableCover


{-| `Row msg columns` upholds every cell from a table's row.
This is a type-safe sized-array.

Use [`Table.rowEnd`](UI-Table#rowEnd) and [`Table.cellFromText`](UI-table#cellFromText) or [`Table.cellFromButton`](UI-table#cellFromButton) to build it.

-}
type alias Row msg columns =
    NList (Cell msg) columns


{-| `HeaderRow msg columns` upholds every label from a table's header.
This is a type-safe sized-array.

Use [`Table.headersEnd`](UI-Table#headersEnd) and [`Table.header`](UI-table#header) to build it.

-}
type alias HeaderRow msg columns =
    NList (HeaderCell msg) columns


{-| `OptRow value columns` upholds option-applying rows.
E.g., `value` is replaced with [`ColumnWidth`](UI-Table#ColumnWidth) for [`Table.withColumnsWidth`](UI-Table#withColumnsWidth).
-}
type alias OptRow value columns =
    NList (Maybe value) columns


type alias HeaderCell msg =
    { title : String
    , width : ColumnWidth
    , detailsVisible : Bool
    , filter : ColumnFilter msg
    }


{-| `ColumnFilter` specifies how filters are applied in a column basis.
-}
type ColumnFilter msg
    = FilterUnavailable
    | FilterEmpty msg -- msg: Toggle
    | FilterEditing (FilterEditConfig msg)
    | FilterSet msg -- msg: Clear


{-| This record aggregates all required rendering information and possible actions when the user is editing some column's filtering field.

    { edited = Just "Bookshelf"
    , applyMsg = Msg.SomeColumnApply
    , clearMsg = Msg.SomeColumnClear
    , closeMsg = Msg.SomeColumnDiscard
    , editMsg = Msg.SomeColumnEdit column
    , current = Just "Book"
    }

-}
type alias FilterEditConfig msg =
    { applyMsg : Maybe msg
    , clearMsg : Maybe msg
    , closeMsg : msg
    , fields : List (FilterField msg)
    }


type FilterField msg
    = FilterText (TextField msg)


type Cell msg
    = CellText Text
    | CellButton (Button msg)


{-| `ColumnWidth` specifies a cell's width.
-}
type ColumnWidth
    = WidthPixels Int
    | WidthPortion Int


{-| Marks the end of a [`HeaderRow`](UI-Table#HeaderRow).

    Table.header "Barcode" <| Table.header "Brand" <| Table.header "Manage" <| Table.headersEnd

-}
headersEnd : HeaderRow msg T.Zero
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
header : String -> HeaderRow msg columns -> HeaderRow msg (T.Increase columns)
header head tail =
    NList.cons
        { title = head
        , width = WidthPortion 1
        , detailsVisible = True
        , filter = FilterUnavailable
        }
        tail


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
table : HeaderRow msg columns -> Table msg object columns
table headers =
    Table { headers = headers } defaultOptions


filterText : TextField msg -> FilterField msg
filterText field =
    FilterText field



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

    Table.withColumnsWidth
        (columnWidthPortion 4 <| columnWidthPortion 5 <| columnsWidthEnd)
        someTwoColumnsTable

-}
withColumnsWidth : OptRow ColumnWidth columns -> Table msg object columns -> Table msg object columns
withColumnsWidth row (Table prop opt_) =
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

    Table.withColumnsDetails
        (columnsMobileDetailsEnd
            |> columnMobileDetailsShow
            -- THIRD COLUMN
            |> columnMobileDetailsShow
            -- SECOND COLUMN
            |> columnMobileDetailsHide
         -- FIRST COLUMN
        )
        someThreeColumnsTable

-}
withColumnsDetails : OptRow Bool columns -> Table msg object columns -> Table msg object columns
withColumnsDetails row (Table prop opt_) =
    let
        mergeVisibility oldHeader maybeVisibility =
            case maybeVisibility of
                Just visibility ->
                    { oldHeader | detailsVisible = visibility }

                Nothing ->
                    oldHeader
    in
    Table { prop | headers = NList.map2 mergeVisibility prop.headers row } opt_


{-| Allows the user to edit filters per column.

    Table.withColumnsFilter
        (columnsFilterEnd
            |> columnFilterEditing
                { edited = Just "Bookshelf"
                , applyMsg = Msg.ColumnCApply
                , clearMsg = Msg.ColumnCClear
                , closeMsg = Msg.ColumnCDiscard
                , editMsg = Msg.ColumnCEdit
                , current = Just "Book"
                }
            |> columnFiltering Msg.ColumnBClear "Dan Brown"
            |> columnFilterEmpty Msg.FilterColumnA
        )
        someThreeColumnsTable

-}
withColumnsFilter : OptRow (ColumnFilter msg) columns -> Table msg object columns -> Table msg object columns
withColumnsFilter row (Table prop opt_) =
    let
        mergeFilter oldHeader maybeVisibility =
            case maybeVisibility of
                Just filter ->
                    { oldHeader | filter = filter }

                Nothing ->
                    oldHeader
    in
    Table { prop | headers = NList.map2 mergeFilter prop.headers row } opt_


{-| Similar to [`Element.fillPortion`](/packages/mdgriffith/elm-ui/latest/Element#fillPortion) but applied to an entire Table's column.

    columnsWidthEnd
        |> columnWidthPortion 20 -- THIRD COLUMN
        |> columnWidthPortion 31 -- SECOND COLUMN
        |> columnWidthPortion 17 -- FIRST COLUMN

-}
columnWidthPortion : Int -> OptRow ColumnWidth columns -> OptRow ColumnWidth (T.Increase columns)
columnWidthPortion int accu =
    opt (WidthPortion int) accu


{-| Similar to [`Element.px`](/packages/mdgriffith/elm-ui/latest/Element#px) but applied to an entire Table's column.

    columnsWidthEnd
        |> columnWidthPixels 320 -- THIRD COLUMN
        |> columnWidthPixels 240 -- SECOND COLUMN
        |> columnWidthPixels 480 -- FIRST COLUMN

-}
columnWidthPixels : Int -> OptRow ColumnWidth columns -> OptRow ColumnWidth (T.Increase columns)
columnWidthPixels int accu =
    opt (WidthPixels int) accu


{-| Marks the end of a [`OptRow ColumnWidth`](UI-Table#ColumnWidth).

    columnWidthPortion 3 <| columnWidthPixels 240 <| columnWidthPortion 2 <| columnsWidthEnd

-}
columnsWidthEnd : OptRow ColumnWidth T.Zero
columnsWidthEnd =
    optsEnd


{-| Marks the end of a [`OptRow ColumnWidth`](UI-Table#ColumnWidth).

    columnsWidthEnd
        |> columnWidthPortion 2 -- THIRD COLUMN
        |> columnWidthPixels 240 -- SECOND COLUMN
        |> columnWidthPortion 3 -- FIRST COLUMN

-}
columnsMobileDetailsEnd : OptRow Bool T.Zero
columnsMobileDetailsEnd =
    optsEnd


{-| Defines that an entire column is to show in the mobile's details of the selected row.

    columnsMobileDetailsEnd
        |> columnMobileDetailsShow -- SHOW THE THIRD COLUMN
        |> columnMobileDetailsHide -- HIDE THE SECOND COLUMN
        |> columnMobileDetailsHide -- HIDE THE FIRST COLUMN

-}
columnMobileDetailsShow : OptRow Bool columns -> OptRow Bool (T.Increase columns)
columnMobileDetailsShow accu =
    opt True accu


{-| Defines that an entire column won't appear in the mobile's details of the selected row.

    columnsMobileDetailsEnd
        |> columnMobileDetailsShow -- SHOW THE THIRD COLUMN
        |> columnMobileDetailsHide -- HIDE THE SECOND COLUMN
        |> columnMobileDetailsHide -- HIDE THE FIRST COLUMN

-}
columnMobileDetailsHide : OptRow Bool columns -> OptRow Bool (T.Increase columns)
columnMobileDetailsHide accu =
    opt False accu


{-| Given a condition, defines that an entire column appears or not in the mobile's details of the selected row.

    columnsMobileDetailsEnd
        |> columnMobileDetailsShow -- SHOW THE THIRD COLUMN
        |> columnMobileDetailsShowIf (isRainingInNewYork model) -- MAYBE HIDE THE SECOND COLUMN
        |> columnMobileDetailsHide -- HIDE THE FIRST COLUMN

-}
columnMobileDetailsShowIf : Bool -> OptRow Bool columns -> OptRow Bool (T.Increase columns)
columnMobileDetailsShowIf condition accu =
    opt condition accu


{-| Marks the end of a [`OptRow ColumnFilter`](UI-Table#ColumnFilter).

    columnFilterEmpty Msg.FilterColumnB <| columnFilterEmpty Msg.FilterColumnA <| columnsFilterEnd

-}
columnsFilterEnd : OptRow (ColumnFilter msg) T.Zero
columnsFilterEnd =
    optsEnd


{-| This column can be filtered, but currently no filter was applied.

    columnsFilterEnd
        |> columnFilterEmpty Msg.FilterColumnC
        |> columnFilterEmpty Msg.FilterColumnB
        |> columnFilterEmpty Msg.FilterColumnA

-}
columnFilterEmpty : msg -> OptRow (ColumnFilter msg) columns -> OptRow (ColumnFilter msg) (T.Increase columns)
columnFilterEmpty toggleMsg accu =
    opt (FilterEmpty toggleMsg) accu


{-| The user is currently editing the value used to filter this column.

    columnsFilterEnd
        |> columnFilterEditing
            { edited = Just "Bookshelf"
            , applyMsg = Msg.ColumnCApply
            , clearMsg = Msg.ColumnCClear
            , closeMsg = Msg.ColumnCDiscard
            , editMsg = Msg.ColumnCEdit
            , current = Just "Book"
            }
        |> columnFilterEmpty Msg.FilterColumnB
        |> columnFilterEmpty Msg.FilterColumnA

-}
columnFilterEditing : FilterEditConfig msg -> OptRow (ColumnFilter msg) columns -> OptRow (ColumnFilter msg) (T.Increase columns)
columnFilterEditing config accu =
    opt (FilterEditing config) accu


{-| This column is currently being filtered.

    columnsFilterEnd
        |> columnFilterEmpty Msg.FilterColumnC
        |> columnFiltering Msg.ColumnBClear
        |> columnFilterEmpty Msg.FilterColumnA

-}
columnFiltering : msg -> OptRow (ColumnFilter msg) columns -> OptRow (ColumnFilter msg) (T.Increase columns)
columnFiltering clearMsg accu =
    opt (FilterSet clearMsg) accu



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


desktopView : RenderConfig -> Bool -> HeaderRow msg columns -> Element.Length -> List (Row msg columns) -> Element msg
desktopView cfg responsive headers width desktopRows =
    let
        rowRender row =
            row
                |> NList.map2 (cellRender cfg) headers
                |> NList.toList
                |> Element.row
                    [ Element.spacing 16
                    , Element.width fill
                    ]
    in
    desktopRows
        |> List.map rowRender
        |> (::)
            (headers
                |> NList.map (headerRender cfg)
                |> NList.toList
                |> Element.row
                    [ Element.spacing 16
                    , Element.width fill
                    , Element.paddingEach { bottom = 7, top = 0, left = 0, right = 0 }
                    , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                    , Border.color Palette.gray.lightest
                    ]
            )
        |> Element.column
            [ Element.spacing 14
            , Element.width width
            , Element.paddingEach <|
                ifThenElse responsive
                    { top = 20, left = 20, right = 20, bottom = 0 }
                    zeroPadding
            ]


mobileView : RenderConfig -> HeaderRow msg columns -> ResponsiveConfig msg object columns -> Element msg
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
    ListView.toggleableList
        { detailsShowLabel = responsiveOpt.detailsShowLabel
        , detailsCollapseLabel = responsiveOpt.detailsCollapseLabel
        , toCover = responsiveOpt.toCover
        , toDetails = details
        , selectMsg = responsiveOpt.selectMsg
        }
        |> ListView.withItems responsiveOpt.items
        |> ListView.withSelected responsiveOpt.isSelected
        |> ListView.renderElement renderConfig


headerRender : RenderConfig -> HeaderCell msg -> Element msg
headerRender cfg { title, width, filter } =
    cellCrop width <|
        case filter of
            FilterUnavailable ->
                simpleHeaderRender cfg title

            FilterEmpty toggleMsg ->
                Button.fromNested title Icon.add
                    |> Button.cmd toggleMsg Button.light
                    |> Button.withWidth Button.widthFull
                    |> Button.withSize Size.small
                    |> Button.renderElement cfg

            FilterEditing filterConfig ->
                filterEditing cfg width title filterConfig

            FilterSet clearMsg ->
                Button.fromNested title Icon.close
                    |> Button.cmd clearMsg Button.primary
                    |> Button.withWidth Button.widthFull
                    |> Button.withSize Size.small
                    |> Button.renderElement cfg


simpleHeaderRender : RenderConfig -> String -> Element msg
simpleHeaderRender cfg title =
    Text.overline title
        |> Text.withColor (Palette.color toneGray brightnessMiddle)
        |> Text.renderElement cfg


cellCrop : ColumnWidth -> Element msg -> Element msg
cellCrop width =
    Element.el
        [ Element.width (widthToEl width)
        , Element.height (shrink |> minimum 1)
        , Element.clipX
        , Element.paddingEach { zeroPadding | bottom = 2 } -- Remove unwanted scrollbars, why tough IDK...
        , Element.alignTop
        ]


filterEditing : RenderConfig -> ColumnWidth -> String -> FilterEditConfig msg -> Element msg
filterEditing cfg width title { clearMsg, applyMsg, closeMsg, fields } =
    Element.column []
        [ overlayBackground closeMsg
        , Element.column
            [ positionFixed
            , zIndex 9
            , Element.width (widthToEl width)
            , Element.alignTop
            , Palette.mainBackground
            , Primitives.roundedBorders
            ]
            [ Element.row
                [ Element.paddingEach { top = 10, left = 12, right = 10, bottom = 7 }
                , Element.width fill
                , Border.color Palette.gray.lighter
                , Border.widthEach { zeroPadding | bottom = 1 }
                ]
                [ Text.overline title
                    |> Text.renderElement cfg
                , Button.fromIcon (Icon.close "Close")
                    |> Button.cmd closeMsg Button.clear
                    |> Button.withSize Size.extraSmall
                    |> Button.renderElement cfg
                ]
            , Element.column
                [ Element.width fill
                , Element.padding 12
                , Element.spacing 20
                ]
                [ fields
                    |> List.map (filterFieldView cfg)
                    |> Element.column
                        [ Element.width fill
                        , Element.spacing 8
                        ]
                , filterEditingButton cfg applyMsg clearMsg
                ]
            ]
        ]


filterFieldView : RenderConfig -> FilterField msg -> Element msg
filterFieldView cfg field =
    case field of
        FilterText textField ->
            textField
                |> TextField.withWidth TextField.widthFull
                |> TextField.renderElement cfg


filterEditingButton : RenderConfig -> Maybe msg -> Maybe msg -> Element msg
filterEditingButton cfg applyMsg clearMsg =
    let
        clearBtn msg =
            Button.fromLabel "Clear"
                |> Button.cmd msg Button.danger
                |> Button.withSize Size.extraSmall
                |> Button.renderElement cfg

        applyBtn msg =
            Button.fromLabel "Apply"
                |> Button.cmd msg Button.primary
                |> Button.withSize Size.extraSmall
                |> Button.renderElement cfg

        disabledBtn =
            Button.fromLabel "Apply"
                |> Button.disabled
                |> Button.withSize Size.extraSmall
                |> Button.renderElement cfg
    in
    case ( applyMsg, clearMsg ) of
        ( Nothing, Nothing ) ->
            disabledBtn

        ( Nothing, Just msg ) ->
            clearBtn msg

        ( Just msg, Nothing ) ->
            applyBtn msg

        ( Just apply, Just clear ) ->
            Element.row [ Element.spacing 8 ] [ applyBtn apply, clearBtn clear ]


overlayBackground : msg -> Element msg
overlayBackground onClickMsg =
    Element.el
        [ positionFixed
        , zIndex 8
        , Palette.overlayBackground
        , Element.htmlAttribute <| HtmlAttrs.style "top" "0"
        , Element.htmlAttribute <| HtmlAttrs.style "left" "0"
        , Element.htmlAttribute <| HtmlAttrs.style "width" "100vw"
        , Element.htmlAttribute <| HtmlAttrs.style "height" "100vh"
        , Events.onClick onClickMsg
        ]
        Element.none


detailRender : RenderConfig -> Cell msg -> Element msg
detailRender cfg cell_ =
    case cell_ of
        CellText text ->
            Text.renderElement cfg text

        CellButton button ->
            Button.renderElement cfg button


cellRender : RenderConfig -> HeaderCell msg -> Cell msg -> Element msg
cellRender cfg { width } cell_ =
    cell_
        |> detailRender cfg
        |> cellCrop width


widthToEl : ColumnWidth -> Element.Length
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


positionFixed : Attribute msg
positionFixed =
    Element.htmlAttribute <| HtmlAttrs.style "position" "fixed"


zIndex : Int -> Attribute msg
zIndex val =
    Element.htmlAttribute <| HtmlAttrs.style "z-index" (String.fromInt val)
