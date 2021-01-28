module UI.Tables.Stateful exposing
    ( StatefulTable, StatefulConfig, table, withItems
    , Responsive, Cover, Details, Detail, withResponsive, detailsEmpty, detailShown, detailHidden
    , State, Msg, init, update, stateWithItems
    , Filters, filtersEmpty, stateWithFilters
    , localSingleTextFilter, remoteSingleTextFilter
    , localMultiTextFilter, remoteMultiTextFilter
    , localSingleDateFilter, remoteSingleDateFilter
    , localRangeDateFilter, remoteRangeDateFilter
    , periodSingle, pariodAfter, periodBefore, localPeriodDateFilter, remotePeriodDateFilter
    , localSelectFilter, remoteSelectFilter
    , withWidth
    , stateWithSelection, stateIsSelected
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

    toTableRow =
        { toKey = .title, toTableRowView}

    toTableRowView { author, title, year } =
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
    , tableState = Stateful.stateWithFilters Book.someFilters Stateful.init
    }


# Stateful

@docs StatefulTable, StatefulConfig, table, withItems


## Mobile

@docs Responsive, Cover, Details, Detail, withResponsive, detailsEmpty, detailShown, detailHidden


## State

@docs State, Msg, init, update, stateWithItems


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

@docs periodSingle, pariodAfter, periodBefore, localPeriodDateFilter, remotePeriodDateFilter


## Select (Radio Buttons)

@docs localSelectFilter, remoteSelectFilter


# Width

@docs withWidth


# Selection


## Local

@docs stateWithSelection, stateIsSelected


## Remote

TODO: withRemoteSelection


# Rendering

@docs renderElement

-}

import Element exposing (Element, fill, px, shrink)
import Element.Keyed as Keyed
import Set exposing (Set)
import Time
import UI.Checkbox as Checkbox exposing (checkbox)
import UI.Internal.Basics exposing (flip, ifThenElse, prependMaybe)
import UI.Internal.DateInput as DateInput exposing (DateInput, PeriodDate, RangeDate)
import UI.Internal.NArray as NArray exposing (NArray)
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Internal.Tables.Common exposing (..)
import UI.Internal.Tables.Filters as Filters
import UI.Internal.Tables.FiltersView as FiltersView
import UI.Internal.Tables.View exposing (..)
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
    , toExternalMsg : Msg item -> msg
    , state : State msg item columns
    }


type alias Options msg item columns =
    { overwriteItems : Maybe (List item)
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
    { overwriteItems = Nothing
    , width = shrink
    , responsive = Nothing
    }


{-| **DEPRECATED**: Use [stateWithItems](#stateWithItems) instead.
Otherwise, by using this you'll be discarding sorting and fitlering.

Each of these items will become a row in this table.

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
    Table prop { opt | overwriteItems = Just items }



-- State


{-| The `Stateful.Msg` handles stateful table's related messages.
-}
type Msg item
    = MobileToggle Int
    | ForFilters Filters.Msg
    | FilterDialogOpen Int
    | FilterDialogClose
    | SelectionToggleAll
    | SelectionSet item Bool


{-| Keep this one in your Model, it holds the table's current state.
-}
type State msg item columns
    = State (StateModel msg item columns)


type alias StateModel msg item columns =
    { filters : Maybe (Filters msg item columns)
    , mobileSelected : Maybe Int
    , filterDialog : Maybe Int
    , localSelection : Maybe (Selection item)
    , items : List item
    , visibleItems : List item
    }


type Selections
    = Individual (Set String)
    | All
    | Except (Set String)


type alias Selection item =
    { identifier : item -> String
    , checks : Selections
    }


{-| The correct way of instantiating a [`Table.State`](#State).

    { -- ...
    , tableState = Stateful.init
    -- ...
    }

-}
init : State msg item columns
init =
    State
        { filters = Nothing
        , mobileSelected = Nothing
        , filterDialog = Nothing
        , localSelection = Nothing
        , items = []
        , visibleItems = []
        }


stateWithItems : List item -> State msg item columns -> State msg item columns
stateWithItems items (State stateModel) =
    State
        { stateModel | items = items, visibleItems = items }


{-| Given a message, apply an update to the [`Table.State`](#State).
Do not ignore the returned `Cmd`, it may include remote filter's messages.

    ( newModel, newCmd ) =
        Table.update subMsg oldModel.tableState

-}
update : Msg item -> State msg item columns -> ( State msg item columns, Cmd msg )
update msg (State state) =
    case msg of
        MobileToggle index ->
            updateMobileToggle state index

        ForFilters subMsg ->
            updateFilters state subMsg

        FilterDialogOpen index ->
            ( State { state | filterDialog = Just index }, Cmd.none )

        FilterDialogClose ->
            ( State { state | filterDialog = Nothing }, Cmd.none )

        SelectionToggleAll ->
            updateSelectionToggleAll state

        SelectionSet item value ->
            updateSelectionSet state item value


updateMobileToggle : StateModel msg item columns -> Int -> ( State msg item columns, Cmd msg )
updateMobileToggle state index =
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


updateFilters : StateModel msg item columns -> Filters.Msg -> ( State msg item columns, Cmd msg )
updateFilters state subMsg =
    case state.filters of
        Just filters ->
            filters
                |> Filters.update subMsg
                |> (\( newFilters, subCmd ) ->
                        ( State { state | filters = Just newFilters }, subCmd )
                   )

        Nothing ->
            ( State state, Cmd.none )


updateSelectionToggleAll : StateModel msg item columns -> ( State msg item columns, Cmd msg )
updateSelectionToggleAll state =
    let
        invertAll old =
            { old
                | checks =
                    case old.checks of
                        Individual _ ->
                            All

                        All ->
                            Individual Set.empty

                        Except set ->
                            if Set.isEmpty set then
                                Individual Set.empty

                            else
                                All
            }
    in
    ( State { state | localSelection = Maybe.map invertAll state.localSelection }
    , Cmd.none
    )


updateSelectionSet : StateModel msg item columns -> item -> Bool -> ( State msg item columns, Cmd msg )
updateSelectionSet state item value =
    let
        setItem old =
            { old
                | checks =
                    case old.checks of
                        Individual set ->
                            selectIndividual old item value set

                        All ->
                            selectExcept old item value Set.empty

                        Except set ->
                            selectExcept old item value set
            }
    in
    ( State { state | localSelection = Maybe.map setItem state.localSelection }
    , Cmd.none
    )


selectIndividual : Selection item -> item -> Bool -> Set String -> Selections
selectIndividual { identifier } item value set =
    set
        |> ifThenElse value
            Set.insert
            Set.remove
            (identifier item)
        |> Individual


selectExcept : Selection item -> item -> Bool -> Set String -> Selections
selectExcept { identifier } item value set =
    set
        |> ifThenElse value
            Set.remove
            Set.insert
            (identifier item)
        |> Except



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



-- Selection


{-| Apply selection defintion to a table's [`State`](#State).

    model =
        stateWithSelection Book.getISBN init

-}
stateWithSelection : (item -> String) -> Bool -> State msg item columns -> State msg item columns
stateWithSelection identifier default (State state) =
    State
        { state
            | localSelection =
                Just
                    { checks =
                        if default then
                            All

                        else
                            Individual Set.empty
                    , identifier = identifier
                    }
        }


{-| Resolves if one item's row is or not selected.

    isHungerGamesSelected =
        Table.stateIsSelected hungerGamesBook tableState

-}
stateIsSelected : item -> State msg item columns -> Bool
stateIsSelected item (State state) =
    case state.localSelection of
        Just selection ->
            internalIsSelected selection item

        Nothing ->
            False



-- Filters


type alias PeriodComparison =
    DateInput.PeriodComparison


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


{-| A filter with one single text field.
Only part of the content must match the filter's input.
Filtering logic is applied internally by the component.

    localSingleTextFilter
        maybeInitialValue
        mapItemToString

-}
localSingleTextFilter :
    Maybe String
    -> (item -> String)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
localSingleTextFilter =
    Filters.singleTextLocal


{-| A filter with one single text field.
Only part of the content must match the filter's input.
Filtering logic is applied through an external message.
When `Nothing` is applied to the message, it means to clear the current filter.

    remoteSingleTextFilter
        maybeInitialValue
        Msg.ApplyFilter

-}
remoteSingleTextFilter :
    Maybe String
    -> (Maybe String -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
remoteSingleTextFilter =
    Filters.singleTextRemote


{-| A filter with multiple text field.
The content must match at least one of those fields otherwise it's filtered out.
Only part of the content must match the filter's input.
Filtering logic is applied internally by the component.

    localMultiTextFilter
        []
        mapItemToString

For having an initial filter applied:

    localMultiTextFilter
        [ "initial", "fields" ]
        mapItemToString

-}
localMultiTextFilter :
    List String
    -> (item -> String)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
localMultiTextFilter =
    Filters.multiTextLocal


{-| A filter with multiple text field.
The content must match at least one of those fields otherwise it's filtered out.
Only part of the content must match the filter's input.

Filtering logic is applied through an external message.
When an empty list is applied to the message, it means to clear the current filter.

    remoteMultiTextFilter
        [ "initial", "fields" ]
        Msg.ApplyFilter

-}
remoteMultiTextFilter :
    List String
    -> (List String -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
remoteMultiTextFilter =
    Filters.multiTextRemote


{-| A filter for dates with one single field.
Filtering logic is applied internally by the component.

    localSingleDateFilter timeZone
        (Just somePosixEpoch)
        mapItemToPosixEpoch

-}
localSingleDateFilter :
    Time.Zone
    -> Maybe Time.Posix
    -> (item -> Time.Posix)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
localSingleDateFilter =
    Filters.singleDateLocal


{-| A filter for dates with one single field.
Filtering logic is applied through an external message.
When `Nothing` is applied to the message, it means to clear the current filter.

    remoteSingleDateFilter
        maybeInitialPosix
        Msg.ApplyFilter

-}
remoteSingleDateFilter :
    Time.Zone
    -> Maybe Time.Posix
    -> (Maybe DateInput -> msg)
    -> Filters.Filters msg item columns
    -> Filters.Filters msg item (T.Increase columns)
remoteSingleDateFilter =
    Filters.singleDateRemote


{-| A filter for dates in an expected range.
The range is defined using two date fields.
Filtering logic is applied internally by the component.

    localRangeDateFilter timeZone
        (Just datesAfterThis)
        (Just datesBeforeThis)
        mapItemToPosixEpoch

**NOTE**: Hours, minutes and seconds are discarded from the range limits.

-}
localRangeDateFilter :
    Time.Zone
    -> Maybe Time.Posix
    -> Maybe Time.Posix
    -> (item -> Time.Posix)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
localRangeDateFilter =
    Filters.rangeDateLocal


{-| A filter for dates in an expected range.
The range is defined using two date fields.

Filtering logic is applied through an external message.
When `Nothing` is applied to the message, it means to clear the current filter.

    remoteRangeDateFilter timeZone
        (Just datesAfterThis)
        (Just datesBeforeThis)
        Msg.ApplyFilter

**NOTE**: Hours, minutes and seconds are discarded from the range limits.

-}
remoteRangeDateFilter :
    Time.Zone
    -> Maybe Time.Posix
    -> Maybe Time.Posix
    -> (Maybe RangeDate -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
remoteRangeDateFilter =
    Filters.rangeDateRemote


{-| A filter for a single date, dates before specified date, or dates after specified date.
The filter-case is defined using radio buttons.
Filtering logic is applied internally by the component.

    localPeriodDateFilter timeZone
        (Just somePosixEpoch)
        (Just periodAfter)
        mapItemToPosixEpoch

**NOTE**: Hours, minutes and seconds are discarded from the range limits.

-}
localPeriodDateFilter :
    Time.Zone
    -> Maybe Time.Posix
    -> Maybe PeriodComparison
    -> (item -> Time.Posix)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
localPeriodDateFilter =
    Filters.periodDateLocal


{-| A filter for a single date, dates before specified date, or dates after specified date.
The filter-case is defined using radio buttons.

Filtering logic is applied through an external message.
When `Nothing` is applied to the message, it means to clear the current filter.

    remotePeriodDateFilter timeZone
        (Just somePosixEpoch)
        (Just periodAfter)
        Msg.ApplyFilter

**NOTE**: Hours, minutes and seconds are discarded from the range limits.

-}
remotePeriodDateFilter :
    Time.Zone
    -> Maybe Time.Posix
    -> Maybe PeriodComparison
    -> (Maybe PeriodDate -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
remotePeriodDateFilter =
    Filters.periodDateRemote


{-| A filter for custom radio buttons.
Filtering logic is applied internally by the component.

    localSelectFilter
        [ "Option 1"
        , "Option 2"
        ]
        (Just 1)
        mapItemEachOptionToBool

-}
localSelectFilter :
    List String
    -> Maybe Int
    -> (item -> Int -> Bool)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
localSelectFilter =
    Filters.selectLocal


{-| A filter for custom radio buttons.

Filtering logic is applied through an external message.
When `Nothing` is applied to the message, it means to clear the current filter.

    remoteSelectFilter
        [ "Option 1"
        , "Option 2"
        ]
        (Just 1)
        Msg.ApplyFilter

-}
remoteSelectFilter :
    List String
    -> Maybe Int
    -> (Maybe Int -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
remoteSelectFilter =
    Filters.selectRemote



-- Periods


{-| When comparing if dates are the same.
-}
periodSingle : PeriodComparison
periodSingle =
    DateInput.On


{-| When comparing if some date is after another.
-}
pariodAfter : PeriodComparison
pariodAfter =
    DateInput.After


{-| When comparing if some date is before another.
-}
periodBefore : PeriodComparison
periodBefore =
    DateInput.Before



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
    let
        detailsTerms =
            renderConfig |> localeTerms >> .tables >> .details

        items =
            viewGetItems (unwrapState prop.state) opt
                |> List.indexedMap Tuple.pair
    in
    ListView.toggleableList
        { detailsShowLabel = detailsTerms.show
        , detailsCollapseLabel = detailsTerms.collapse
        , toKey = Tuple.second >> prop.toRow.toKey
        , toCover = Tuple.second >> responsive.toCover
        , toDetails =
            Tuple.second
                >> responsive.toDetails
                >> NArray.toList
                >> List.filterMap (Maybe.map (detailView renderConfig))
        , selectMsg = Tuple.first >> MobileToggle >> prop.toExternalMsg
        }
        |> ListView.withItems items
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
            unwrapState prop.state

        items =
            viewGetItems state opt

        rows =
            List.map (rowWithSelection renderConfig prop.toExternalMsg state prop.toRow columns) items

        padding =
            { top = 20, left = 20, right = 20, bottom = 0 }

        selectionHeader =
            viewSelectionHeader renderConfig state prop.toExternalMsg

        headers =
            headersRender renderConfig
                prop.toExternalMsg
                state.filterDialog
                state.filters
                columns
                selectionHeader
    in
    Keyed.column
        [ Element.spacing 2
        , Element.width opt.width
        , Element.paddingEach padding
        ]
        (headers :: rows)


viewSelectionHeader : RenderConfig -> StateModel msg item columns -> (Msg item -> msg) -> Maybe (Element msg)
viewSelectionHeader renderConfig state toExternalMsg =
    Maybe.map
        (SelectionToggleAll
            |> toExternalMsg
            |> FiltersView.headerSelectToggle renderConfig
            |> always
        )
        state.localSelection


viewGetItems : StateModel msg item columns -> Options msg item columns -> List item
viewGetItems { visibleItems } opt =
    Maybe.withDefault visibleItems opt.overwriteItems


unwrapState : State msg item columns -> StateModel msg item columns
unwrapState (State model) =
    model


rowWithSelection :
    RenderConfig
    -> (Msg item -> msg)
    -> StateModel msg item columns
    -> ToRow msg item columns
    -> List Column
    -> item
    -> ( String, Element msg )
rowWithSelection renderConfig msgMap state toRow columns item =
    rowBox <|
        case state.localSelection of
            Just selection ->
                item
                    |> rowRender renderConfig toRow columns
                    |> Tuple.mapSecond
                        ((::)
                            (item
                                |> selectionCell renderConfig selection
                                |> Tuple.mapSecond (Element.map msgMap)
                            )
                        )

            Nothing ->
                rowRender renderConfig toRow columns item


headersRender :
    RenderConfig
    -> (Msg item -> msg)
    -> Maybe Int
    -> Maybe (Filters.Filters msg item columns)
    -> List Column
    -> Maybe (Element msg)
    -> ( String, Element msg )
headersRender renderConfig toExternalMsg selected filters columns selectionHeader =
    ( "@headers"
    , Element.row headersAttr <|
        case filters of
            Just filterArr ->
                filterArr
                    |> NArray.toList
                    |> List.map2 (filterHeader renderConfig toExternalMsg selected) columns
                    |> List.indexedMap (\index val -> val index)
                    |> prependMaybe selectionHeader

            Nothing ->
                List.map
                    (\(Column header { width }) ->
                        header
                            |> simpleHeaderRender renderConfig
                            |> cellSpace width
                    )
                    columns
    )


filterHeader :
    RenderConfig
    -> (Msg item -> msg)
    -> Maybe Int
    -> Column
    -> Filters.Filter msg item
    -> Int
    -> Element msg
filterHeader renderConfig toExternalMsg selected (Column header { width }) filter index =
    FiltersView.header renderConfig
        filter
        { openMsg = toExternalMsg <| FilterDialogOpen index
        , discardMsg = toExternalMsg <| FilterDialogClose
        , fromFiltersMsg = ForFilters >> toExternalMsg
        , index = index
        , label = header
        , isOpen = selected == Just index
        }
        |> topCellSpace width



-- Selection


internalIsSelected : Selection item -> item -> Bool
internalIsSelected { identifier, checks } item =
    case checks of
        Individual set ->
            Set.member (identifier item) set

        All ->
            True

        Except set ->
            set
                |> Set.member (identifier item)
                |> not


selectionCell : RenderConfig -> Selection item -> item -> ( String, Element (Msg item) )
selectionCell renderConfig selection item =
    item
        |> internalIsSelected selection
        |> checkbox "Select row" (SelectionSet item)
        |> Checkbox.withLabelVisible False
        |> Checkbox.renderElement renderConfig
        |> Element.el
            [ Element.centerX, Element.centerY ]
        |> Element.el
            [ Element.width (px 32), Element.height fill ]
        |> Tuple.pair "@select"
