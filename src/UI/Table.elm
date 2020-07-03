module UI.Table exposing
    ( Table, table, withItems
    , Columns, columnsEmpty, columnsPushHeader, columnsPush
    , Column, headerToColumn, columnWidthPortion, columnWidthPixels
    , Row, rowEmpty, rowPushText, rowPushButton
    , Responsive, Cover, Details, Detail, withResponsive, detailsEmpty, detailsPush, detailsPushHidden
    , Cell, cellFromText, cellFromButton
    , State, Msg, withState, stateInit, stateUpdate
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
        |> Table.withWidth (Element.fill |> Element.maximum 640)
        |> Table.withFilters someFilters
        |> Table.withItems
            [ Book "Dan Brown" "Angels & Demons" "2000"
            , Book "Dan Brown" "The Da Vinci Code" "2003"
            , Book "Dan Brown" "The Lost Symbol" "2009"
            , Book "Dan Brown" "Inferno" "2013"
            , Book "Dan Brown" "Origin" "2017"
            ]
        |> Table.renderElement renderConfig

Where `Book` is:

    type alias Book =
        { author : String, title : String, year : String }

    tableColumns =
        columnsEmpty
            |> columnsPush (headerToColumn "Title" |> columnWidthPixels 320)
            |> columnsPush (headerToColumn "Author" |> columnWidthPixels 240)
            |> columnsPush (headerToColumn "Year" |> columnWidthPixels 120)

    toTableRow { author, title, year } =
        rowEmpty
            |> rowPushText (Text.body1 title)
            |> rowPushText (Text.body2 author)
            |> rowPushText (Text.caption year)

    toTableDetails { author, title } =
        detailsEmpty
            |> detailsPushHidden
            |> detailsPush { label = "Author", content = cellFromText <| Text.body2 author }
            |> detailsPushHidden

    toTableCover { title, year } =
        { title = title, caption = Just year }

    someFilters =
        filtersEmpty
            |> filtersPushSingleText Nothing (filterLocal (\{ title } str -> String.contains str title))
            |> filtersPushSingleText Nothing (filterRemote { editMsg = Msg.FilterAuthor })
            |> filtersPushSingleText Nothing (filterLocal (\{ year } str -> String.contains str year))


# Table

@docs Table, table, withItems


## Desktop

@docs Columns, columnsEmpty, columnsPushHeader, columnsPush


## Individual column

@docs Column, headerToColumn, columnWidthPortion, columnWidthPixels


## Desktop rows

@docs Row, rowEmpty, rowPushText, rowPushButton


## Mobile

@docs Responsive, Cover, Details, Detail, withResponsive, detailsEmpty, detailsPush, detailsPushHidden


## Individual cell

@docs Cell, cellFromText, cellFromButton


## State

@docs State, Msg, withState, stateInit, stateUpdate


# Filters

@docs Filters, withFilters, filtersEmpty, filtersPushSingleText


## Filter's Strategy

@docs Strategy, filterLocal, filterRemote


# Width

@docs withWidth


# Rendering

@docs renderElement

-}

import Element exposing (Attribute, Element, fill, fillPortion, minimum, px, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Html.Attributes as HtmlAttrs
import UI.Button as Button exposing (Button)
import UI.Icon as Icon
import UI.Internal.Basics exposing (ifThenElse, swap)
import UI.Internal.Filters as Filters
import UI.Internal.NArray as NArray exposing (NArray)
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Internal.TypeNumbers as T
import UI.ListView as ListView
import UI.Palette as Palette exposing (brightnessMiddle, toneGray)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text exposing (Text)
import UI.TextField as TextField
import UI.Utils.Element exposing (zeroPadding)


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


withItems : List item -> Table msg item columns -> Table msg item columns
withItems items (Table prop opt) =
    Table prop { opt | items = items }



-- State


type Msg
    = MobileToggle Int
    | ForFilters Filters.Msg
    | FilterDialogOpen Int
    | FilterDialogClose


type State
    = State StateModel


type alias StateModel =
    { filters : Filters.Model
    , mobileSelected : Maybe Int
    , filterDialog : Maybe Int
    }


stateInit : State
stateInit =
    State { filters = Filters.init, mobileSelected = Nothing, filterDialog = Nothing }


stateUpdate : Msg -> State -> State
stateUpdate msg (State state) =
    case msg of
        MobileToggle index ->
            State
                { state
                    | mobileSelected =
                        if state.mobileSelected == Just index then
                            Nothing

                        else
                            Just index
                }

        ForFilters subMsg ->
            State { state | filters = Filters.update subMsg state.filters }

        FilterDialogOpen index ->
            State { state | filterDialog = Just index }

        FilterDialogClose ->
            State { state | filterDialog = Nothing }


withState : State -> Table msg item columns -> Table msg item columns
withState state (Table prop opt) =
    Table prop { opt | state = Just state }



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



-- Cells


type Cell msg
    = CellText Text
    | CellButton (Button msg)


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
rowPushText : Text -> Row msg columns -> Row msg (T.Increase columns)
rowPushText text accu =
    NArray.push (CellText text) accu


{-| Transforms a `UI.Button` into a cell appending it to a row.

TODO: Example

-}
rowPushButton : Button msg -> Row msg columns -> Row msg (T.Increase columns)
rowPushButton btn accu =
    NArray.push (CellButton btn) accu



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
    Maybe String
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


{-| Applies [`Element.width`](/packages/mdgriffith/elm-ui/latest/Element#width) to the component.

    Table.withWidth
        (Element.fill |> Element.minimum 220)
        someTable

-}
withWidth : Element.Length -> Table msg item columns -> Table msg item columns
withWidth width (Table prop opt_) =
    Table prop { opt_ | width = width }



-- Rendering


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Table msg item columns -> Element msg
renderElement renderConfig (Table prop opt) =
    case opt.responsive of
        Just responsive ->
            if RenderConfig.isMobile renderConfig then
                mobileView renderConfig prop opt responsive

            else
                desktopView renderConfig prop opt

        Nothing ->
            desktopView renderConfig prop opt


cellContentRender : RenderConfig -> Cell msg -> Element msg
cellContentRender renderConfig cell_ =
    case cell_ of
        CellText text ->
            Text.renderElement renderConfig text

        CellButton button ->
            Button.renderElement renderConfig button


widthToEl : ColumnWidth -> Element.Length
widthToEl width =
    case width of
        WidthPortion int ->
            fillPortion int

        WidthPixels int ->
            px int



-- Mobile rendering


mobileView :
    RenderConfig
    -> Properties msg item columns
    -> Options msg item columns
    -> Responsive msg item columns
    -> Element msg
mobileView renderConfig prop opt responsive =
    ListView.toggleableList
        { detailsShowLabel = "Expand"
        , detailsCollapseLabel = "Collapse"
        , toCover = Tuple.second >> responsive.toCover
        , toDetails =
            Tuple.second
                >> responsive.toDetails
                >> NArray.toList
                >> List.filterMap (Maybe.map (detailView renderConfig))
        , selectMsg = Tuple.first >> MobileToggle >> prop.toExtern
        }
        |> ListView.withItems (List.indexedMap Tuple.pair opt.items)
        |> ListView.withSelected (Tuple.first >> isSelected opt.state)
        |> ListView.renderElement renderConfig


isSelected : Maybe State -> Int -> Bool
isSelected state position =
    case state of
        Nothing ->
            False

        Just (State { mobileSelected }) ->
            Just position == mobileSelected


detailView : RenderConfig -> Detail msg -> ( String, Element msg )
detailView renderConfig { label, content } =
    ( label, cellContentRender renderConfig content )



-- Dekstop rendering


desktopView :
    RenderConfig
    -> Properties msg item columns
    -> Options msg item columns
    -> Element msg
desktopView renderConfig prop opt =
    let
        len =
            NArray.length prop.columns

        indexedList =
            List.range 0 (len - 1)

        columns =
            NArray.toList prop.columns

        filters =
            opt.filters
                |> Maybe.map (NArray.map Just >> NArray.toList)
                |> Maybe.withDefault (List.repeat len Nothing)

        filtersState =
            case opt.state of
                Just (State state) ->
                    List.map
                        (\index ->
                            ( index
                            , Filters.get index state.filters
                            , state.filterDialog == Just index
                            )
                        )
                        indexedList

                Nothing ->
                    List.map
                        (\index ->
                            ( index, Nothing, False )
                        )
                        indexedList

        headers =
            headersRender renderConfig prop.toExtern filters filtersState columns

        rows =
            List.map (rowRender renderConfig prop.toRow columns) opt.items

        padding =
            case opt.responsive of
                Just _ ->
                    { top = 20, left = 20, right = 20, bottom = 0 }

                Nothing ->
                    zeroPadding
    in
    Element.column
        [ Element.spacing 14
        , Element.width opt.width
        , Element.paddingEach padding
        ]
        (headers :: rows)


headersRender :
    RenderConfig
    -> (Msg -> msg)
    -> List (Maybe (Filters.Filter msg item))
    -> List ( Int, Maybe Filters.FilterModel, Bool )
    -> List Column
    -> Element msg
headersRender renderConfig toExtern filters filtersState columns =
    Element.row
        [ Element.spacing 16
        , Element.width fill
        , Element.paddingEach { bottom = 7, top = 0, left = 0, right = 0 }
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color Palette.gray.lightest
        ]
        (List.map3 (headerRender renderConfig toExtern)
            filters
            filtersState
            columns
        )


rowRender : RenderConfig -> ToRow msg item columns -> List Column -> item -> Element msg
rowRender renderConfig toRow columns item =
    toRow item
        |> NArray.toList
        |> List.map2 (cellRender renderConfig) columns
        |> Element.row
            [ Element.spacing 16
            , Element.width fill
            , Element.mouseOver [ Background.color Palette.gray.lightest ]
            ]


cellRender : RenderConfig -> Column -> Cell msg -> Element msg
cellRender renderConfig (Column _ { width }) cell =
    cell
        |> cellContentRender renderConfig
        |> cellCrop width


cellCrop : ColumnWidth -> Element msg -> Element msg
cellCrop width =
    Element.el
        [ Element.width (widthToEl width)
        , Element.height (shrink |> minimum 1)
        , Element.paddingEach { zeroPadding | bottom = 2 }
        , Element.clipX
        , Element.alignTop
        ]



-- Headers


headerRender :
    RenderConfig
    -> (Msg -> msg)
    -> Maybe (Filters.Filter msg item)
    -> ( Int, Maybe Filters.FilterModel, Bool )
    -> Column
    -> Element msg
headerRender renderConfig toExtern maybeFilter ( index, maybeFilterState, isFilterOpen ) (Column header { width }) =
    cellCrop width <|
        case maybeFilterState of
            Nothing ->
                noFilterStateHeader renderConfig toExtern maybeFilter isFilterOpen index width header

            Just (Filters.SingleTextModel editable) ->
                if isFilterOpen then
                    singleTextFilterRender renderConfig toExtern index width header editable

                else
                    simpleHeaderRender renderConfig header

            _ ->
                simpleHeaderRender renderConfig header


noFilterStateHeader :
    RenderConfig
    -> (Msg -> msg)
    -> Maybe (Filters.Filter msg item)
    -> Bool
    -> Int
    -> ColumnWidth
    -> String
    -> Element msg
noFilterStateHeader renderConfig toExtern maybeFilter isFilterOpen index width header =
    case maybeFilter of
        Just (Filters.SingleTextFilter { init, strategy }) ->
            if isFilterOpen then
                init
                    |> Filters.editableInit
                    |> singleTextFilterRender renderConfig toExtern index width header

            else
                closedFilteredHeader renderConfig toExtern index header

        _ ->
            simpleHeaderRender renderConfig header


simpleHeaderRender : RenderConfig -> String -> Element msg
simpleHeaderRender renderConfig header =
    header
        |> String.toUpper
        |> Text.overline
        |> Text.withColor (Palette.color toneGray brightnessMiddle)
        |> cellFromText
        |> cellContentRender renderConfig


closedFilteredHeader : RenderConfig -> (Msg -> msg) -> Int -> String -> Element msg
closedFilteredHeader renderConfig toExtern index header =
    Button.fromNested header Icon.add
        |> Button.cmd (toExtern <| FilterDialogOpen index) Button.light
        |> Button.withWidth Button.widthFull
        |> Button.withSize Size.small
        |> Button.renderElement renderConfig


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


filterEditingButton : RenderConfig -> msg -> msg -> Filters.Editable data -> Element msg
filterEditingButton cfg applyMsg clearMsg { applied, current } =
    let
        clearBtn =
            Button.fromLabel "Clear"
                |> Button.cmd clearMsg Button.danger
                |> Button.withSize Size.extraSmall
                |> Button.renderElement cfg

        applyBtn =
            Button.fromLabel "Apply"
                |> Button.cmd applyMsg Button.primary
                |> Button.withSize Size.extraSmall
                |> Button.renderElement cfg

        disabledBtn =
            Button.fromLabel "Apply"
                |> Button.disabled
                |> Button.withSize Size.extraSmall
                |> Button.renderElement cfg
    in
    case ( applied, current ) of
        ( _, Just _ ) ->
            Element.row [ Element.spacing 8 ] [ applyBtn, clearBtn ]

        ( Just _, Nothing ) ->
            clearBtn

        ( Nothing, Nothing ) ->
            disabledBtn


filterEditRender :
    RenderConfig
    -> (Msg -> msg)
    -> Int
    -> Filters.FilterModel
    -> ColumnWidth
    -> String
    -> Filters.Editable data
    -> Element msg
    -> Element msg
filterEditRender renderConfig toExtern index empty width header editable content =
    let
        discardMsg =
            toExtern FilterDialogClose

        applyMsg =
            toExtern <| ForFilters <| Filters.Apply index

        clearMsg =
            toExtern <| ForFilters <| Filters.Set index empty
    in
    Element.column []
        [ overlayBackground discardMsg
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
                [ Text.overline header
                    |> Text.renderElement renderConfig
                , Button.fromIcon (Icon.close "Close")
                    |> Button.cmd discardMsg Button.clear
                    |> Button.withSize Size.extraSmall
                    |> Button.renderElement renderConfig
                ]
            , Element.column
                [ Element.width fill
                , Element.padding 12
                , Element.spacing 20
                ]
                [ content
                , filterEditingButton renderConfig applyMsg clearMsg editable
                ]
            ]
        ]


singleTextFilterRender :
    RenderConfig
    -> (Msg -> msg)
    -> Int
    -> ColumnWidth
    -> String
    -> Filters.Editable String
    -> Element msg
singleTextFilterRender renderConfig toExtern index width header editable =
    let
        editMsg str =
            toExtern <| ForFilters <| Filters.EditSingleText { column = index, value = str }

        empty =
            Filters.SingleTextModel Filters.editableEmpty
    in
    editable
        |> Filters.editableDefault ""
        |> TextField.singlelineText editMsg header
        |> TextField.withWidth TextField.widthFull
        |> TextField.renderElement renderConfig
        |> filterEditRender renderConfig toExtern index empty width header editable



-- CSS: Not to be used anywhere else


positionFixed : Attribute msg
positionFixed =
    Element.htmlAttribute <| HtmlAttrs.style "position" "fixed"


zIndex : Int -> Attribute msg
zIndex val =
    Element.htmlAttribute <| HtmlAttrs.style "z-index" (String.fromInt val)
