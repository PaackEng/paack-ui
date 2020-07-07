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
    -> (String -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
remoteSingleTextFilter initValue editMsg accu =
    Filters.remoteSingleTextFilter initValue editMsg accu



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
            List.map2 mergeFilter filters filtersState
                |> List.filterMap identity
                |> List.foldl reduceFilters (always True)

        headers =
            headersRender renderConfig prop.toExternalMsg filters filtersState columns

        filteredItems =
            List.filter mergedFilters opt.items

        rows =
            List.map (rowRender renderConfig prop.toRow columns) filteredItems

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



-- Headers


headerRender :
    RenderConfig
    -> (Msg -> msg)
    -> Maybe (Filters.Filter msg item)
    -> ( Int, Maybe Filters.FilterModel, Bool )
    -> Column
    -> Element msg
headerRender renderConfig toExternalMsg maybeFilter ( index, maybeFilterState, isFilterOpen ) (Column header { width }) =
    cellSpace width <|
        case maybeFilterState of
            Nothing ->
                noFilterStateHeader renderConfig toExternalMsg maybeFilter isFilterOpen index width header

            Just (Filters.SingleTextModel editable) ->
                filterStateHeader renderConfig toExternalMsg singleTextFilterRender Filters.singleTextEmpty isFilterOpen index width header editable

            _ ->
                simpleHeaderRender renderConfig header


noFilterStateHeader :
    RenderConfig
    -> (Msg -> msg)
    -> Maybe (Filters.Filter msg item)
    -> Bool
    -> Int
    -> Common.ColumnWidth
    -> String
    -> Element msg
noFilterStateHeader renderConfig toExternalMsg maybeFilter isFilterOpen index width header =
    case maybeFilter of
        Just (Filters.SingleTextFilter { initial }) ->
            if isFilterOpen then
                initial
                    |> Filters.editableInit
                    |> singleTextFilterRender renderConfig toExternalMsg index width header

            else if initial /= Nothing then
                appliedFilterRender renderConfig toExternalMsg Filters.singleTextEmpty index header

            else
                closedFilteredHeader renderConfig toExternalMsg index header

        _ ->
            simpleHeaderRender renderConfig header


closedFilteredHeader : RenderConfig -> (Msg -> msg) -> Int -> String -> Element msg
closedFilteredHeader renderConfig toExternalMsg index header =
    let
        openMsg =
            toExternalMsg <| FilterDialogOpen index
    in
    FiltersHeaders.normal renderConfig openMsg header


overlayBackground : msg -> Element msg
overlayBackground onClickMsg =
    Element.el
        [ positionFixed -- Needs for starting at the top-left corner
        , zIndex 8
        , Palette.overlayBackground
        , Element.htmlAttribute <| HtmlAttrs.style "top" "0"
        , Element.htmlAttribute <| HtmlAttrs.style "left" "0"
        , Element.htmlAttribute <| HtmlAttrs.style "width" "100vw"
        , Element.htmlAttribute <| HtmlAttrs.style "height" "100vh"
        , Events.onClick onClickMsg
        ]
        Element.none


type alias FilterStateRenderer msg value =
    RenderConfig
    -> (Msg -> msg)
    -> Int
    -> Common.ColumnWidth
    -> String
    -> Filters.Editable value
    -> Element msg


filterStateHeader :
    RenderConfig
    -> (Msg -> msg)
    -> FilterStateRenderer msg value
    -> Filters.FilterModel
    -> Bool
    -> Int
    -> Common.ColumnWidth
    -> String
    -> Filters.Editable value
    -> Element msg
filterStateHeader renderConfig toExternalMsg renderer empty isFilterOpen index width header editable =
    if isFilterOpen then
        renderer renderConfig toExternalMsg index width header editable

    else if editable.applied /= Nothing then
        appliedFilterRender renderConfig toExternalMsg empty index header

    else
        closedFilteredHeader renderConfig toExternalMsg index header


appliedFilterRender : RenderConfig -> (Msg -> msg) -> Filters.FilterModel -> Int -> String -> Element msg
appliedFilterRender renderConfig toExternalMsg empty index header =
    let
        clearMsg =
            toExternalMsg <| ForFilters <| Filters.Set index empty

        openMsg =
            toExternalMsg <| FilterDialogOpen index
    in
    FiltersHeaders.applied renderConfig openMsg clearMsg "Clear" header


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
    -> Common.ColumnWidth
    -> String
    -> Filters.Editable data
    -> Element msg
    -> Element msg
filterEditRender renderConfig toExternalMsg index empty width header editable content =
    let
        discardMsg =
            toExternalMsg FilterDialogClose

        applyMsg =
            toExternalMsg <| ForFilters <| Filters.Apply index

        clearMsg =
            toExternalMsg <| ForFilters <| Filters.Set index empty
    in
    Element.el
        [ Element.width fill
        , Element.height (shrink |> minimum 1)
        , Element.inFront <|
            -- Using inFront is required for cell's proportional width
            Element.column
                [ Element.width fill
                , zIndex 9
                , Element.alignTop
                , Palette.mainBackground
                , Primitives.defaultRoundedBorders
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
        (overlayBackground discardMsg)


singleTextFilterRender : FilterStateRenderer msg String
singleTextFilterRender renderConfig toExternalMsg index width header editable =
    let
        editMsg str =
            toExternalMsg <| ForFilters <| Filters.EditSingleText { column = index, value = str }

        empty =
            Filters.singleTextEmpty
    in
    editable
        |> Filters.editableDefault ""
        |> TextField.singlelineText editMsg header
        |> TextField.withWidth TextField.widthFull
        |> TextField.renderElement renderConfig
        |> filterEditRender renderConfig toExternalMsg index empty width header editable



-- Filter logic


mergeFilter :
    Maybe (Filters.Filter msg item)
    -> ( Int, Maybe Filters.FilterModel, Bool )
    -> Maybe (item -> Bool)
mergeFilter filter ( _, model, _ ) =
    Maybe.andThen (flip Filters.localFilterGet model) filter


reduceFilters : (item -> Bool) -> (item -> Bool) -> (item -> Bool)
reduceFilters new old =
    \item -> new item && old item



-- CSS: Not to be used anywhere else


positionFixed : Attribute msg
positionFixed =
    Element.htmlAttribute <| HtmlAttrs.style "position" "fixed"


zIndex : Int -> Attribute msg
zIndex val =
    Element.htmlAttribute <| HtmlAttrs.style "z-index" (String.fromInt val)
