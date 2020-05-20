module SafeTables exposing (stories)

import Element exposing (Element, fill)
import UI.RenderConfig exposing (RenderConfig)
import UI.SafeTable as Table exposing (cellPortion, header, headersEnd, opt, optKeep, optsEnd, rowEnd, table)
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
            header "HEADER A" <| header "HEADER B" <| header "HEADER C" <| headersEnd

        rows =
            [ cell "CELL A1" <| cell "CELL B1" <| cell "CELL C1" <| rowEnd
            , cell "CELL A2" <| cell "CELL B2" <| cell "CELL C2" <| rowEnd
            , cell "CELL A3" <| cell "CELL B3" <| cell "CELL C3" <| rowEnd
            ]

        cellsWidth =
            optKeep <| opt (cellPortion 4) <| optKeep <| optsEnd

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
            header "NAME" <| header "COUNTRY" <| header "AGE" <| header "ACTION" <| headersEnd

        rows =
            [ cell "Pedro Campos" <| cell "Brazil" <| cell "24" <| cell "Action" <| rowEnd
            ]

        cellsWidth =
            opt (cellPortion 4) <| opt (cellPortion 2) <| opt (cellPortion 2) <| optKeep <| optsEnd

        cell str =
            Text.body1 str |> Table.cellFromText
    in
    table headers
        |> Table.withRows rows
        |> Table.withWidth fill
        |> Table.withCellsWidth cellsWidth
        |> Table.toEl cfg
