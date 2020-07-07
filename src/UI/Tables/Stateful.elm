module UI.Tables.Stateful exposing
    ( StatefulTable, StatefulConfig, table, withItems
    , Responsive, Cover, Details, Detail, withResponsive, detailsEmpty, detailShown, detailHidden
    , State, Msg, init, update
    , Filters, withFilters, filtersEmpty
    , localSingleTextFilter, remoteSingleTextFilter
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
        |> Stateful.withFilters someFilters
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


# Stateful

@docs StatefulTable, StatefulConfig, table, withItems


## Mobile

@docs Responsive, Cover, Details, Detail, withResponsive, detailsEmpty, detailShown, detailHidden


## State

@docs State, Msg, init, update


# Filters

@docs Filters, withFilters, filtersEmpty


## Single Text

@docs localSingleTextFilter, remoteSingleTextFilter


# Width

@docs withWidth


# Rendering

@docs renderElement

-}

import Element exposing (Attribute, Element, fill, minimum, shrink)
import Element.Border as Border
import Element.Events as Events
import Html.Attributes as HtmlAttrs
import UI.Button as Button
import UI.Icon as Icon
import UI.Internal.Basics exposing (flip)
import UI.Internal.Filters as Filters
import UI.Internal.FiltersHeaders as FiltersHeaders
import UI.Internal.NArray as NArray exposing (NArray)
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Internal.Table exposing (..)
import UI.Internal.TableView exposing (..)
import UI.Internal.TypeNumbers as T
import UI.ListView as ListView
import UI.Palette as Palette
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Tables.Common as Common exposing (..)
import UI.Text as Text
import UI.TextField as TextField
import UI.Utils.Element exposing (zeroPadding)


{-| The `StatefulTable msg item columns` type is used for describing the component for later rendering.
-}
type StatefulTable msg item columns
    = Table (StatefulConfig msg item columns) (Options msg item columns)


type alias StatefulConfig msg item columns =
    { columns : Columns columns
    , toRow : ToRow msg item columns
    , toExternalMsg : Msg -> msg
    , state : State
    }


type alias Options msg item columns =
    { items : List item
    , filters : Maybe (Filters msg item columns)
    , width : Element.Length
    , responsive : Maybe (Responsive msg item columns)
    }


table : StatefulConfig msg item columns -> StatefulTable msg item columns
table config =
    Table config defaultOptions


defaultOptions : Options msg item columns
defaultOptions =
    { items = []
    , filters = Nothing
    , width = shrink
    , responsive = Nothing
    }


withItems : List item -> StatefulTable msg item columns -> StatefulTable msg item columns
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


init : State
init =
    State { filters = Filters.init, mobileSelected = Nothing, filterDialog = Nothing }


update : Msg -> State -> State
update msg (State state) =
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



-- Responsive


type alias Responsive msg item columns =
    { toDetails : item -> Details msg columns
    , toCover : item -> Cover
    }


type alias Cover =
    ListView.ToggleableCover


type alias Detail msg =
    { label : String, content : Common.Cell msg }


type alias Details msg columns =
    NArray (Maybe (Detail msg)) columns


withResponsive : Responsive msg item columns -> StatefulTable msg item columns -> StatefulTable msg item columns
withResponsive responsive (Table prop opt) =
    Table prop { opt | responsive = Just responsive }


detailsEmpty : Details msg T.Zero
detailsEmpty =
    NArray.empty


detailShown : Detail msg -> Details msg columns -> Details msg (T.Increase columns)
detailShown detail accu =
    NArray.push (Just detail) accu


detailHidden : Details msg columns -> Details msg (T.Increase columns)
detailHidden accu =
    NArray.push Nothing accu



-- Filters


type alias Filters msg item columns =
    Filters.Filters msg item columns


type alias Strategy msgs value item =
    Filters.Strategy msgs value item


withFilters : Filters msg item columns -> StatefulTable msg item columns -> StatefulTable msg item columns
withFilters filters (Table prop opt) =
    Table prop { opt | filters = Just filters }


filtersEmpty : Filters msg item T.Zero
filtersEmpty =
    Filters.filtersEmpty


localSingleTextFilter :
    Maybe String
    -> (item -> String)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
localSingleTextFilter initValue get accu =
    Filters.localSingleTextFilter initValue get accu


remoteSingleTextFilter :
    Maybe String
    -> (Maybe String -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
remoteSingleTextFilter initValue applyMsg accu =
    Filters.remoteSingleTextFilter initValue applyMsg accu



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


isSelected : State -> Int -> Bool
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
            case prop.state of
                State state ->
                    List.map
                        (\index ->
                            ( index
                            , Filters.get index state.filters
                            , state.filterDialog == Just index
                            )
                        )
                        indexedList

        mergedFilters =
            List.map2 Filters.filtersMerge filters filtersState
                |> List.filterMap identity
                |> List.foldl Filters.filtersReduce (always True)

        headers =
            headersRender renderConfig prop.toExternalMsg filters filtersState columns

        rows =
            opt.items
                |> List.filter mergedFilters
                |> List.map (rowRender renderConfig prop.toRow columns)

        padding =
            case opt.responsive of
                Just _ ->
                    { top = 20, left = 20, right = 20, bottom = 0 }

                Nothing ->
                    zeroPadding
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
    -> List (Maybe (Filters.Filter msg item))
    -> List ( Int, Maybe Filters.FilterModel, Bool )
    -> List Column
    -> Element msg
headersRender renderConfig toExternalMsg filters filtersState columns =
    Element.row
        headersAttr
        (List.map3 (headerRender renderConfig toExternalMsg)
            filters
            filtersState
            columns
        )


headerRender :
    RenderConfig
    -> (Msg -> msg)
    -> Maybe (Filters.Filter msg item)
    -> ( Int, Maybe Filters.FilterModel, Bool )
    -> Column
    -> Element msg
headerRender renderConfig toExternalMsg maybeFilter ( index, maybeFilterState, isFilterOpen ) (Column header { width }) =
    cellSpace width <|
        case maybeFilter of
            Nothing ->
                simpleHeaderRender renderConfig header

            Just filter ->
                let
                    config =
                        { openMsg = toExternalMsg <| FilterDialogOpen index
                        , discardMsg = toExternalMsg <| FilterDialogClose
                        , fromFiltersMsg = ForFilters >> toExternalMsg
                        , index = index
                        , label = header
                        , isOpen = isFilterOpen
                        }
                in
                FiltersHeaders.header renderConfig filter maybeFilterState config
