module Tables.Stories exposing (stories, update)

import Element exposing (Element, fill)
import Msg
import Return as R exposing (Return)
import Tables.Model as Tables
import Tables.Msg as Tables
import UI.Internal.TypeNumbers as T
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Table as Table
    exposing
        ( cellFromText
        , cellMobileDetailsEnd
        , cellMobileDetailsHide
        , cellMobileDetailsShow
        , cellWidthEnd
        , cellWidthPortion
        , cellWidthShrink
        , header
        , headersEnd
        , rowEnd
        , table
        )
import UI.Text as Text exposing (Text)
import UIExplorer exposing (storiesOf)
import Utils exposing (iconsSvgSprite, story, storyWithModel)


update : Tables.Msg -> Tables.Model -> Return Tables.Msg Tables.Model
update msg model =
    case msg of
        Tables.Select newValue ->
            ( { model | selected = Just newValue }, Cmd.none )


stories cfg =
    storiesOf
        "Safe Tables"
        [ staticTableStory cfg
        , responsiveTableStory mobileCfg
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

        cellsWidth =
            cellWidthPortion 2 <| cellWidthShrink <| cellWidthShrink <| cellWidthPortion 2 <| cellWidthEnd

        cell str =
            Text.body1 str |> Table.cellFromText
    in
    table headers
        |> Table.withStaticRows rows
        |> Table.withWidth fill
        |> Table.withCellsWidth cellsWidth
        |> Table.toEl cfg
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

        cellsWidth =
            cellWidthPortion 4 <| cellWidthPortion 1 <| cellWidthEnd

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
            cellMobileDetailsEnd
                |> cellMobileDetailsShow
                |> cellMobileDetailsHide

    in
    table headers
        |> Table.withResponsiveRows responsiveOpt
        |> Table.withWidth fill
        |> Table.withCellsWidth cellsWidth
        |> Table.withCellsDetails mobileDetails
        |> Table.toEl cfg
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

        cellsWidth =
            cellWidthPortion 2 <| cellWidthShrink <| cellWidthShrink <| cellWidthPortion 2 <| cellWidthEnd

        cell str =
            Text.body1 str |> cellFromText
    in
    table headers
        |> Table.withStaticRows rows
        |> Table.withWidth fill
        |> Table.withCellsWidth cellsWidth
        |> Table.toEl cfg


responsiveTable cfg { selected } =
    let
        headers =
            header "Name" <| header "Birthday" <| headersEnd

        items =
            [ { name = "John", birthday = "23/11/2013" }
            , { name = "Paul", birthday = "04/02/1969" }
            ]

        cellsWidth =
            cellWidthPortion 4 <| cellWidthPortion 1 <| cellWidthEnd

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
            cellMobileDetailsEnd
                |> cellMobileDetailsShow
                |> cellMobileDetailsHide
    in
    table headers
        |> Table.withResponsiveRows responsiveOpt
        |> Table.withWidth fill
        |> Table.withCellsWidth cellsWidth
        |> Table.withCellsDetails mobileDetails
        |> Table.toEl cfg
        |> List.singleton
        |> (::) iconsSvgSprite
        |> Element.wrappedRow [ Element.width fill ]


mobileCfg =
    RenderConfig.fromWindow { width = 375, height = 667 }
