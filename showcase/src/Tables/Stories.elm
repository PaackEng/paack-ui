module Tables.Stories exposing (stories, update)

import Element exposing (Element, fill)
import Msg
import Return as R exposing (Return)
import Tables.Model as Tables
import Tables.Msg as Tables
import UI.Internal.Basics exposing (maybeNotThen)
import UI.Internal.TypeNumbers as T
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Table as Table
    exposing
        ( cellFromText
        , columnFilterEditing
        , columnFilterEmpty
        , columnFiltering
        , columnMobileDetailsHide
        , columnMobileDetailsShow
        , columnWidthPixels
        , columnWidthPortion
        , columnsFilterEnd
        , columnsMobileDetailsEnd
        , columnsWidthEnd
        , header
        , headersEnd
        , rowEnd
        , table
        )
import UI.Text as Text exposing (Text)
import UI.TextField as TextField
import UIExplorer exposing (storiesOf)
import Utils exposing (iconsSvgSprite, story, storyWithModel)


update : Tables.Msg -> Tables.Model -> Return Tables.Msg Tables.Model
update msg model =
    case msg of
        Tables.Select newValue ->
            ( { model | selected = Just newValue }, Cmd.none )

        Tables.FilterApply ->
            case model.editing of
                Just ( column, Just value ) ->
                    apply column (Just value) model

                _ ->
                    ( model, Cmd.none )

        Tables.FilterClear column ->
            apply column Nothing model

        Tables.FilterEdit column value ->
            ( { model | editing = Just ( column, value ) }
            , Cmd.none
            )

        Tables.FilterDiscard ->
            ( { model | editing = Nothing }
            , Cmd.none
            )


apply : Tables.Column -> Maybe String -> Tables.Model -> Return Tables.Msg Tables.Model
apply column value model =
    case column of
        Tables.ColumnA ->
            let
                oldFilters =
                    model.filters

                newFilters =
                    { oldFilters | columnA = value }
            in
            ( { model | filters = newFilters, editing = Nothing }
            , Cmd.none
            )

        Tables.ColumnB ->
            let
                oldFilters =
                    model.filters

                newFilters =
                    { oldFilters | columnB = value }
            in
            ( { model | filters = newFilters, editing = Nothing }
            , Cmd.none
            )

        Tables.ColumnC ->
            let
                oldFilters =
                    model.filters

                newFilters =
                    { oldFilters | columnC = value }
            in
            ( { model | filters = newFilters, editing = Nothing }
            , Cmd.none
            )

        Tables.ColumnD ->
            let
                oldFilters =
                    model.filters

                newFilters =
                    { oldFilters | columnD = value }
            in
            ( { model | filters = newFilters, editing = Nothing }
            , Cmd.none
            )


stories cfg =
    storiesOf
        "Safe Tables"
        [ staticTableStory cfg
        , responsiveTableStory mobileCfg
        , filteredTableStory cfg
        ]


staticTableStory cfg =
    story
        ( "Simple Table"
        , staticTable cfg
        , { note = """```elm
staticTable : RenderConfig -> Element msg
staticTable cfg =
    let
        headers =
            header "HEADER A" <| header "HEADER B" <| header "HEADER C" <| header "HEADER D" <| headersEnd

        rows =
            [ cell "FIRST-CELL A1" <| cell "CELL B1" <| cell "CELL C1" <| cell "LAST-CELL D1" <| rowEnd
            , cell "FIRST-CELL A2" <| cell "CELL B2" <| cell "CELL C2" <| cell "LAST-CELL D2" <| rowEnd
            , rowEnd -- Always in reverse so it's O(1) and not O(N)
                |> cell "LAST-CELL D3"
                |> cell "CELL C3"
                |> cell "CELL B3"
                |> cell "FIRST-CELL A3"
            ]

        columnsWidth =
            columnWidthPortion 2 <| columnWidthPortion 1 <| columnWidthPortion 1 <| columnWidthPortion 2 <| columnsWidthEnd

        cell str =
            Text.body1 str |> Table.cellFromText
    in
    table headers
        |> Table.withStaticRows rows
        |> Table.withWidth fill
        |> Table.withColumnsWidth columnsWidth
        |> Table.renderElement cfg
```"""
          }
        )


responsiveTableStory cfg =
    storyWithModel
        ( "Responsive Table"
        , \{ tablesStories } -> responsiveTable cfg tablesStories
        , { note = """```elm
responsiveTable : RenderConfig -> Maybe { name : String, birthday: String } -> Element Msg
responsiveTable cfg selectedSomeone =
    let
        headers =
            header "Name" <| header "Birthday" <| headersEnd

        items =
            [ {name = "John", birthday = "23/11/2013"}
            , {name = "Paul", birthday = "04/02/1969"}
            ]

        columnsWidth =
            columnWidthPortion 4 <| columnWidthPortion 1 <| columnsWidthEnd

        cell str =
            Text.body1 str |> cellFromText

        rowMap {name, birthday} =
            cell name <| cell birthday <| rowEnd

        responsiveOpt =
            { detailsShowLabel = "Show details"
            , detailsCollapseLabel = "Hide details"
            , toRow = rowMap
            , toCover = mobileCover
            , selectMsg = Msg.SelectSomeone
            , isSelected = isSelected
            , items = items
            }

        isSelected { name } =
            selectedSomeone
                |> Maybe.map (.name >> (==) name)
                |> Maybe.withDefault False


        mobileCover { name } =
            { title = name, caption = Nothing }

        mobileDetails =
            columnsMobileDetailsEnd
                |> columnMobileDetailsShow
                |> columnMobileDetailsHide

    in
    table headers
        |> Table.withResponsiveRows responsiveOpt
        |> Table.withWidth fill
        |> Table.withColumnsWidth columnsWidth
        |> Table.withColumnsDetails mobileDetails
        |> Table.renderElement cfg
```"""
          }
        )


filteredTableStory cfg =
    storyWithModel
        ( "Filtered Table"
        , \{ tablesStories } -> filteredTable cfg tablesStories
        , { note = """```elm
        -- There are many ways to implement this in your model, so here it's a generic example:
        columnsFilters =
            (columnsFilterEnd
                |> columnFilterEmpty Msg.FilterColumnD
                |> columnFilterEditing
                    { applyMsg = Just Msg.ColumnCApply
                    , clearMsg = Just Msg.ColumnCClear
                    , discardMsg = Msg.ColumnCDiscard
                    , fields =
                        [ model.currentCFilter
                            |>TextField.singlelineText
                                Msg.ColumnCSet
                                "Some text field"
                            |> Table.filterText
                        ]
                    }
                |> columnFiltering Msg.ColumnBClear
                |> columnFilterEmpty Msg.FilterColumnA
            )
    in
    table headers
        |> Table.withStaticRows rows
        |> Table.withWidth fill
        |> Table.withColumnsWidth columnsWidth
        |> Table.withColumnsFilter columnsFilters
        |> Table.renderElement cfg
```"""
          }
        )


staticTable cfg =
    let
        headers =
            header "HEADER A" <| header "HEADER B" <| header "HEADER C" <| header "HEADER D" <| headersEnd

        rows =
            [ cell "FIRST-CELL A1" <| cell "CELL B1" <| cell "CELL C1" <| cell "LAST-CELL D1" <| rowEnd
            , cell "FIRST-CELL A2" <| cell "CELL B2" <| cell "CELL C2" <| cell "LAST-CELL D2" <| rowEnd
            , rowEnd
                -- Always in reverse so it's O(1) and not O(N)
                |> cell "LAST-CELL D3"
                |> cell "CELL C3"
                |> cell "CELL B3"
                |> cell "FIRST-CELL A3"
            ]

        columnsWidth =
            columnWidthPortion 2 <| columnWidthPortion 1 <| columnWidthPortion 1 <| columnWidthPortion 2 <| columnsWidthEnd

        cell str =
            Text.body1 str |> cellFromText
    in
    table headers
        |> Table.withStaticRows rows
        |> Table.withWidth fill
        |> Table.withColumnsWidth columnsWidth
        |> Table.renderElement cfg


responsiveTable cfg { selected } =
    let
        headers =
            header "Name" <| header "Birthday" <| headersEnd

        items =
            [ { name = "John", birthday = "23/11/2013" }
            , { name = "Paul", birthday = "04/02/1969" }
            ]

        columnsWidth =
            columnWidthPortion 4 <| columnWidthPortion 1 <| columnsWidthEnd

        cell str =
            Text.body1 str |> cellFromText

        rowMap { name, birthday } =
            cell name <| cell birthday <| rowEnd

        responsiveOpt =
            { detailsShowLabel = "Show details"
            , detailsCollapseLabel = "Hide details"
            , toRow = rowMap
            , toCover = mobileCover
            , selectMsg = Msg.TablesStoriesMsg << Tables.Select << .name
            , isSelected = isSelected
            , items = items
            }

        isSelected { name } =
            selected
                |> Maybe.map ((==) name)
                |> Maybe.withDefault False

        mobileCover { name } =
            { title = name, caption = Nothing }

        mobileDetails =
            columnsMobileDetailsEnd
                |> columnMobileDetailsShow
                |> columnMobileDetailsHide
    in
    table headers
        |> Table.withResponsiveRows responsiveOpt
        |> Table.withWidth fill
        |> Table.withColumnsWidth columnsWidth
        |> Table.withColumnsDetails mobileDetails
        |> Table.renderElement cfg
        |> List.singleton
        |> (::) iconsSvgSprite
        |> Element.wrappedRow [ Element.width fill ]


mobileCfg =
    RenderConfig.fromWindow { width = 375, height = 667 }


filteredTable cfg model =
    let
        headers =
            header "HEADER A" <| header "HEADER B" <| header "HEADER C" <| header "HEADER D" <| headersEnd

        rows =
            [ cell "FIRST-CELL A1" <| cell "CELL B1" <| cell "CELL C1" <| cell "LAST-CELL D1" <| rowEnd
            , cell "FIRST-CELL A2" <| cell "CELL B2" <| cell "CELL C2" <| cell "LAST-CELL D2" <| rowEnd
            , rowEnd
                -- Always in reverse so it's O(1) and not O(N)
                |> cell "LAST-CELL D3"
                |> cell "CELL C3"
                |> cell "CELL B3"
                |> cell "FIRST-CELL A3"
            , cell "FIRST-CELL A4" <| cell "CELL B4" <| cell "CELL C4" <| cell "LAST-CELL D4" <| rowEnd
            , cell "FIRST-CELL A5" <| cell "CELL B5" <| cell "CELL C5" <| cell "LAST-CELL D5" <| rowEnd
            ]

        columnsWidth =
            columnWidthPixels 145 <| columnWidthPixels 130 <| columnWidthPixels 130 <| columnWidthPixels 145 <| columnsWidthEnd

        filterEdit column edited current =
            { applyMsg =
                Maybe.map
                    (always <| Msg.TablesStoriesMsg <| Tables.FilterApply)
                    edited
            , clearMsg =
                if current /= Nothing || edited /= Nothing then
                    Just <| Msg.TablesStoriesMsg <| Tables.FilterClear column

                else
                    Nothing
            , closeMsg =
                Msg.TablesStoriesMsg <| Tables.FilterDiscard
            , fields =
                [ TextField.singlelineText
                    (Just >> Tables.FilterEdit column >> Msg.TablesStoriesMsg)
                    "Some text field"
                    (edited |> maybeNotThen current |> Maybe.withDefault "")
                    |> Table.filterText
                ]
            }

        columnFilter column =
            case grapColumn column model of
                Just edited ->
                    Tables.getColumnFilter column model
                        |> filterEdit column edited
                        |> columnFilterEditing

                Nothing ->
                    case Tables.getColumnFilter column model of
                        Just current ->
                            Tables.FilterClear column
                                |> Msg.TablesStoriesMsg
                                |> columnFiltering

                        Nothing ->
                            columnFilterEmpty (Msg.TablesStoriesMsg <| Tables.FilterEdit column Nothing)

        columnsFilter =
            columnsFilterEnd
                |> columnFilter Tables.ColumnD
                |> columnFilter Tables.ColumnC
                |> columnFilter Tables.ColumnB
                |> columnFilter Tables.ColumnA

        cell str =
            Text.body1 str |> cellFromText
    in
    table headers
        |> Table.withStaticRows rows
        |> Table.withColumnsWidth columnsWidth
        |> Table.withColumnsFilter columnsFilter
        |> Table.renderElement cfg
        |> List.singleton
        |> (::) iconsSvgSprite
        |> Element.wrappedRow [ Element.width fill ]


grapColumn column { editing } =
    case editing of
        Just ( editingColumn, edited ) ->
            if editingColumn == column then
                Just edited

            else
                Nothing

        Nothing ->
            Nothing
