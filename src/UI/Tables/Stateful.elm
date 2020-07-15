module UI.Tables.Stateful exposing
    ( StatefulTable, StatefulConfig, table, withItems
    , Responsive, Cover, Details, Detail, withResponsive, detailsEmpty, detailShown, detailHidden
    , State, Msg, init, update
    , Filters, filtersEmpty, stateWithFilters
    , localSingleTextFilter, remoteSingleTextFilter
    , localMultiTextFilter, remoteMultiTextFilter
    , localSingleDateFilter, remoteSingleDateFilter
    , localRangeDateFilter, remoteRangeDateFilter
    , localPeriodDateFilter, remotePeriodDateFilter
    , localSelectFilter, remoteSelectFilter
    , withWidth
    , renderElement
    )

{-| Tables are a matrixial data disposition with rows, columns, headers, and cells.

`UI.Tables` are type-safe, which means that every row needs to have the same number of columns (including the headers). Otherwise, compilation fails.

    Stateful.table
        { toExternalMsg = Msg.ForTable
        , columns = Book.tableColumns
        , toRow = Book.toTableRow
        , state = model.tableState
        }
        |> Stateful.withResponsive
            { toDetails = Book.toTableDetails
            , toCover = Book.toTableCover
            }
        |> Stateful.withWidth (Element.fill |> Element.maximum 640)
        |> Stateful.withItems
            [ Book "Dan Brown" "Angels & Demons" "2000"
            , Book "Dan Brown" "The Da Vinci Code" "2003"
            , Book "Dan Brown" "The Lost Symbol" "2009"
            , Book "Dan Brown" "Inferno" "2013"
            , Book "Dan Brown" "Origin" "2017"
            ]
        |> Stateful.renderElement renderConfig

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

    toTableDetails { author, title } =
        detailsEmpty
            |> detailHidden
            |> detailShown { label = "Author", content = cellFromText <| Text.body2 author }
            |> detailHidden

    toTableCover { title, year } =
        { title = title, caption = Just year }

    someFilters =
        filtersEmpty
            |> localSingleTextFilter Nothing .title
            |> localSingleTextFilter (Just "Dan") .author
            |> localSingleTextFilter Nothing .year

And on model:

    { -...
    , tableState : Stateful.Table Msg.Msg TypeNumbers.Three
    }

    { -...
    , tableState = Stateful.withFilters Book.someFilters Stateful.init
    }


# Stateful

@docs StatefulTable, StatefulConfig, table, withItems


## Mobile

@docs Responsive, Cover, Details, Detail, withResponsive, detailsEmpty, detailShown, detailHidden


## State

@docs State, Msg, init, update


# Filters

@docs Filters, filtersEmpty, stateWithFilters


## Single Text

@docs localSingleTextFilter, remoteSingleTextFilter


## Multi Text

@docs localMultiTextFilter, remoteMultiTextFilter


## Single DateInput

@docs localSingleDateFilter, remoteSingleDateFilter


## Range Dates

@docs localRangeDateFilter, remoteRangeDateFilter


## Period Dates

@docs localPeriodDateFilter, remotePeriodDateFilter


## Select (Radio Buttons)

@docs localSelectFilter, remoteSelectFilter


# Width

@docs withWidth


# Rendering

@docs renderElement

-}

import Element exposing (Element, shrink)
import Time
import UI.Internal.Basics exposing (flip)
import UI.Internal.DateInput exposing (DateInput, PeriodComparison, PeriodDate, RangeDate)
import UI.Internal.Filters as Filters
import UI.Internal.FiltersHeaders as FiltersHeaders
import UI.Internal.NArray as NArray exposing (NArray)
import UI.Internal.Table exposing (..)
import UI.Internal.TableView exposing (..)
import UI.ListView as ListView
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Tables.Common as Common exposing (..)
import UI.Utils.TypeNumbers as T


{-| The `StatefulTable msg item columns` type is used for describing the component for later rendering.

This is type that constrains type-safe sized-arrays.
See [`TypeNumbers`](UI-Utils-TypeNumbers) for how to compose its phantom type.

-}
type StatefulTable msg item columns
    = Table (StatefulConfig msg item columns) (Options msg item columns)


{-| Record with parameters for the creation of a [`StatefulTable`](#table).

This is record that constrains type-safe sized-arrays.
See [`TypeNumbers`](UI-Utils-TypeNumbers) for how to compose its phantom type.

    { toExternalMsg = Msg.ForTable
    , columns = Book.tableColumns
    , toRow = Book.toTableRow
    , state = model.tableState
    }

-}
type alias StatefulConfig msg item columns =
    { columns : Columns columns
    , toRow : ToRow msg item columns
    , toExternalMsg : Msg -> msg
    , state : State msg item columns
    }


type alias Options msg item columns =
    { items : List item
    , width : Element.Length
    , responsive : Maybe (Responsive msg item columns)
    }


{-| Constructs a stateful table from its columns and rows.
Also defines the handling function for messages, and the current table's state.

    table
        { columns = Book.tableColumns
        , toRow = Book.toTableRow
        , toExternalMsg = Msg.ForTable
        , state = model.tableState
        }

-}
table : StatefulConfig msg item columns -> StatefulTable msg item columns
table config =
    Table config defaultOptions


defaultOptions : Options msg item columns
defaultOptions =
    { items = []
    , width = shrink
    , responsive = Nothing
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
withItems : List item -> StatefulTable msg item columns -> StatefulTable msg item columns
withItems items (Table prop opt) =
    Table prop { opt | items = items }



-- State


{-| The `Stateful.Msg` handles stateful table's related messages.
-}
type Msg
    = MobileToggle Int
    | ForFilters Filters.Msg
    | FilterDialogOpen Int
    | FilterDialogClose


{-| Keep this one in your Model, it holds the table's current state.
-}
type State msg item columns
    = State (StateModel msg item columns)


type alias StateModel msg item columns =
    { filters : Maybe (Filters msg item columns)
    , mobileSelected : Maybe Int
    , filterDialog : Maybe Int
    }


{-| The correct way of instantiating a [`Table.State`](#State).

    { -- ...
    , tableState = Stateful.init
    -- ...
    }

-}
init : State msg item columns
init =
    State { filters = Nothing, mobileSelected = Nothing, filterDialog = Nothing }


{-| Given a message, apply an update to the [`Table.State`](#State).
Do not ignore the returned `Cmd`, it may include remote filter's messages.

    ( newModel, newCmd ) =
        Table.update subMsg oldModel.tableState

-}
update : Msg -> State msg item columns -> ( State msg item columns, Cmd msg )
update msg ((State state) as model) =
    case msg of
        MobileToggle index ->
            ( State
                { state
                    | mobileSelected =
                        if state.mobileSelected == Just index then
                            Nothing

                        else
                            Just index
                }
            , Cmd.none
            )

        ForFilters subMsg ->
            case state.filters of
                Just filters ->
                    filters
                        |> Filters.update subMsg
                        |> (\( newFilters, subCmd ) ->
                                ( State { state | filters = Just newFilters }, subCmd )
                           )

                Nothing ->
                    ( model, Cmd.none )

        FilterDialogOpen index ->
            ( State { state | filterDialog = Just index }, Cmd.none )

        FilterDialogClose ->
            ( State { state | filterDialog = Nothing }, Cmd.none )



-- Responsive


{-| Required information for displaying the mobile's layout.

This is record that constrains type-safe sized-arrays.
See [`TypeNumbers`](UI-Utils-TypeNumbers) for how to compose its phantom type.

    { toDetails = Book.toTableDetails
    , toCover = Book.toTableCover
    }

-}
type alias Responsive msg item columns =
    { toDetails : item -> Details msg columns
    , toCover : item -> Cover
    }


{-| What is displayed in a collapsed mobile's row.

    { title = "Foo Fighters - Everlong"
    , caption = Just "Morumbi - São Paulo 2015-01-23"
    }

-}
type alias Cover =
    ListView.ToggleableCover


{-| Used to render a cell in the mobile's layout.
-}
type alias Detail msg =
    { label : String, content : Common.Cell msg }


{-| A set of [`Detail msg`](#Detail).
Must have the same amount of elements as cells do in the table's row.
-}
type alias Details msg columns =
    NArray (Maybe (Detail msg)) columns


{-| Allows a table to have a responsive layout when on mobile.

    withResponsive
        { toDetails = Book.toTableDetails
        , toCover = Book.toTableCover
        }
        someTable

-}
withResponsive : Responsive msg item columns -> StatefulTable msg item columns -> StatefulTable msg item columns
withResponsive responsive (Table prop opt) =
    Table prop { opt | responsive = Just responsive }


{-| An empty [`Details`](#Details) set.

    toTableDetails { author, title } =
        detailsEmpty
            |> detailHidden
            |> detailShown
                { label = "Author"
                , content = cellFromText (Text.body2 author)
                }
            |> detailHidden

-}
detailsEmpty : Details msg T.Zero
detailsEmpty =
    NArray.empty


{-| Defines that a cell will be shown in the mobile's layout.

    detailShown
        { label = "Edit"
        , content = cellFromButton editButton
        }
        detailsSet

-}
detailShown : Detail msg -> Details msg columns -> Details msg (T.Increase columns)
detailShown detail accu =
    NArray.push (Just detail) accu


{-| Defines that a cell will be hidden in the mobile's layout.

    detailsEmpty
        |> detailHidden
        |> detailHidden
        |> detailHidden

-}
detailHidden : Details msg columns -> Details msg (T.Increase columns)
detailHidden accu =
    NArray.push Nothing accu



-- Filters


{-| Array with all the columns' filters and their initial state.

This is a type-safe sized-array.
See [`TypeNumbers`](UI-Utils-TypeNumbers) for how to compose its phantom type.

-}
type alias Filters msg item columns =
    Filters.Filters msg item columns


{-| Apply filters defintion to a table's [`State`](#State).

    model =
        stateWithFilters Book.filtersInit init

-}
stateWithFilters : Filters msg item columns -> State msg item columns -> State msg item columns
stateWithFilters filters (State state) =
    State { state | filters = Just filters }


{-| An empty [`Filters`](#Filters) set.

    toTableDetails { author, title } =
        filtersEmpty
            |> localSingleTextFilter Nothing .title
            |> localSingleTextFilter (Just "Dan") .author
            |> localSingleTextFilter Nothing .year

-}
filtersEmpty : Filters msg item T.Zero
filtersEmpty =
    Filters.empty


localSingleTextFilter :
    Maybe String
    -> (item -> String)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
localSingleTextFilter =
    Filters.singleTextLocal


remoteSingleTextFilter :
    Maybe String
    -> (Maybe String -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
remoteSingleTextFilter =
    Filters.singleTextRemote


localMultiTextFilter :
    List String
    -> (item -> String)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
localMultiTextFilter =
    Filters.multiTextLocal


remoteMultiTextFilter :
    List String
    -> (List String -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
remoteMultiTextFilter =
    Filters.multiTextRemote


localSingleDateFilter :
    Time.Zone
    -> Maybe Time.Posix
    -> (item -> Time.Posix)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
localSingleDateFilter =
    Filters.singleDateLocal


remoteSingleDateFilter :
    Time.Zone
    -> Maybe Time.Posix
    -> (Maybe DateInput -> msg)
    -> Filters.Filters msg item columns
    -> Filters.Filters msg item (T.Increase columns)
remoteSingleDateFilter =
    Filters.singleDateRemote


localRangeDateFilter :
    Time.Zone
    -> Maybe Time.Posix
    -> Maybe Time.Posix
    -> (item -> Time.Posix)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
localRangeDateFilter =
    Filters.rangeDateLocal


remoteRangeDateFilter :
    Time.Zone
    -> Maybe Time.Posix
    -> Maybe Time.Posix
    -> (Maybe RangeDate -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
remoteRangeDateFilter =
    Filters.rangeDateRemote


localPeriodDateFilter :
    Time.Zone
    -> Maybe Time.Posix
    -> Maybe PeriodComparison
    -> (item -> Time.Posix)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
localPeriodDateFilter =
    Filters.periodDateLocal


remotePeriodDateFilter :
    Time.Zone
    -> Maybe Time.Posix
    -> Maybe PeriodComparison
    -> (Maybe PeriodDate -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
remotePeriodDateFilter =
    Filters.periodDateRemote


localSelectFilter :
    List String
    -> Maybe Int
    -> (item -> Int -> Bool)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
localSelectFilter =
    Filters.selectLocal


remoteSelectFilter :
    List String
    -> Maybe Int
    -> (Maybe Int -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
remoteSelectFilter =
    Filters.selectRemote



-- Width


{-| Applies [`Element.width`](/packages/mdgriffith/elm-ui/latest/Element#width) to the component.

    Table.withWidth
        (Element.fill |> Element.minimum 220)
        someTable

-}
withWidth : Element.Length -> StatefulTable msg item columns -> StatefulTable msg item columns
withWidth width (Table prop opt_) =
    Table prop { opt_ | width = width }



-- Rendering


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> StatefulTable msg item columns -> Element msg
renderElement renderConfig (Table prop opt) =
    case opt.responsive of
        Just responsive ->
            if RenderConfig.isMobile renderConfig then
                mobileView renderConfig prop opt responsive

            else
                desktopView renderConfig prop opt

        Nothing ->
            desktopView renderConfig prop opt



-- Mobile rendering


mobileView :
    RenderConfig
    -> StatefulConfig msg item columns
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
        , selectMsg = Tuple.first >> MobileToggle >> prop.toExternalMsg
        }
        |> ListView.withItems (List.indexedMap Tuple.pair opt.items)
        |> ListView.withSelected (Tuple.first >> isSelected prop.state)
        |> ListView.renderElement renderConfig


isSelected : State msg item columns -> Int -> Bool
isSelected (State { mobileSelected }) position =
    Just position == mobileSelected


detailView : RenderConfig -> Detail msg -> ( String, Element msg )
detailView renderConfig { label, content } =
    ( label, cellContentRender renderConfig content )



-- Dekstop rendering


desktopView :
    RenderConfig
    -> StatefulConfig msg item columns
    -> Options msg item columns
    -> Element msg
desktopView renderConfig prop opt =
    let
        columns =
            NArray.toList prop.columns

        state =
            case prop.state of
                State state_ ->
                    state_

        items =
            case state.filters of
                Just filtersArr ->
                    filtersArr
                        |> NArray.toList
                        |> List.filterMap Filters.filterGet
                        |> List.foldl Filters.filtersReduce (always True)
                        |> flip List.filter opt.items

                Nothing ->
                    opt.items

        rows =
            List.map (rowRender renderConfig prop.toRow columns) items

        padding =
            { top = 20, left = 20, right = 20, bottom = 0 }

        headers =
            headersRender renderConfig
                prop.toExternalMsg
                state.filterDialog
                state.filters
                columns
    in
    Element.column
        [ Element.spacing 2
        , Element.width opt.width
        , Element.paddingEach padding
        ]
        (headers :: rows)


headersRender :
    RenderConfig
    -> (Msg -> msg)
    -> Maybe Int
    -> Maybe (Filters.Filters msg item columns)
    -> List Column
    -> Element msg
headersRender renderConfig toExternalMsg selected filters columns =
    Element.row headersAttr <|
        case filters of
            Just filterArr ->
                filterArr
                    |> NArray.toList
                    |> List.map2 (filterHeader renderConfig toExternalMsg selected) columns
                    |> List.indexedMap (\index val -> val index)

            Nothing ->
                List.map
                    (\(Column header { width }) ->
                        header
                            |> simpleHeaderRender renderConfig
                            |> cellSpace width
                    )
                    columns


filterHeader :
    RenderConfig
    -> (Msg -> msg)
    -> Maybe Int
    -> Column
    -> Filters.Filter msg item
    -> Int
    -> Element msg
filterHeader renderConfig toExternalMsg selected (Column header { width }) filter index =
    FiltersHeaders.header renderConfig
        filter
        { openMsg = toExternalMsg <| FilterDialogOpen index
        , discardMsg = toExternalMsg <| FilterDialogClose
        , fromFiltersMsg = ForFilters >> toExternalMsg
        , index = index
        , label = header
        , isOpen = selected == Just index
        }
        |> topCellSpace width
