module SafeTables exposing (stories)

import Element exposing (Element, fill)
import UI.RenderConfig exposing (RenderConfig)
import UI.SafeTable as Table
    exposing
        ( cellWidthEnd
        , cellWidthPortion
        , cellWidthShrink
        , header
        , headersEnd
        , rowEnd
        , table
        )
import UI.Text as Text exposing (Text)
import UIExplorer exposing (storiesOf)
import Utils exposing (story)


stories cfg =
    storiesOf
        "Safe Tables"
        [ example cfg
        ]


example cfg =
    story
        ( "Simple Table"
        , simpleTable cfg
        , { note = """```elm
simpleTable : RenderConfig -> Element msg
simpleTable cfg =
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
        |> Table.withRows rows
        |> Table.withWidth fill
        |> Table.withCellsWidth cellsWidth
        |> Table.toEl cfg
```"""
          }
        )


simpleTable : RenderConfig -> Element msg
simpleTable cfg =
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
            Text.body1 str |> Table.cellFromText
    in
    table headers
        |> Table.withRows rows
        |> Table.withWidth fill
        |> Table.withCellsWidth cellsWidth
        |> Table.toEl cfg
