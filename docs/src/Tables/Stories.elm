module Tables.Stories exposing
    ( stories
    , update
    )

import Element as El
import Msg as RootMsg
import Return as R exposing (Return)
import Tables.Model as Model exposing (Model)
import Tables.Msg as TablesMsg
import UI.Table as Table
import UIExplorer exposing (storiesOf)
import Utils exposing (storyWithModel)


update : TablesMsg.Msg -> Model -> Return TablesMsg.Msg Model
update msg model =
    case msg of
        TablesMsg.Msg tableMsg ->
            Table.update tableMsg Table.noEvents model.defaultTable
                |> R.map (\state -> { model | defaultTable = state })
                |> R.mapCmd TablesMsg.Msg


stories =
    storiesOf
        "Tables"
        [ defaultStory
        , leftAlignedStory
        ]


defaultStory =
    storyWithModel
        ( "Default"
        , \model ->
            defaultTableView
                |> Table.toEl model.tablesStories.defaultTable
        , { note = """
```elm
    type alias Player =
        { number : Int
        , name : String
        , position : String
        }

    state =
        Table.initState .name
    header =
        [ "Number"
        , "Name"
        , "Position"
        ]
        |> List.map El.text
    body =
        [ \\{ number } -> El.text (String.fromInt number)
        , \\{ name } -> El.text name
        , \\{ position } -> El.text position
        ]
    tableView =
        players
            |> Table.default YourTableMsg
            |> Table.withHeader header
            |> Table.withBody body
            |> Table.toEl state
```
"""
          }
        )


leftAlignedStory =
    storyWithModel
        ( "With text alignment"
        , \model ->
            defaultTableView
                |> Table.withTextAlignedToLeft
                |> Table.toEl model.tablesStories.defaultTable
        , { note = """

You can use `withTextAlignedToLeft`, `withTextAlignedToCenter` and `withTextAlignedToRight` to change the text alignment of the table.

---

```elm
    type alias Player =
        { number : Int
        , name : String
        , position : String
        }

    state =
        Table.initState .name
    header =
        [ "Number"
        , "Name"
        , "Position"
        ]
        |> List.map El.text
    body =
        [ \\{ number } -> El.text (String.fromInt number)
        , \\{ name } -> El.text name
        , \\{ position } -> El.text position
        ]
    tableView =
        players
            |> Table.default YourTableMsg
            |> Table.withHeader header
            |> Table.withBody body
            |> Table.withTextAlignedToLeft
            |> Table.toEl state
```
"""
          }
        )


header : List (El.Element msg)
header =
    [ "Number"
    , "Name"
    , "Position"
    ]
        |> List.map El.text


body : List (Model.Player -> El.Element msg)
body =
    [ \{ number } -> El.text (String.fromInt number)
    , \{ name } -> El.text name
    , \{ position } -> El.text position
    ]


defaultTableView =
    Model.players
        |> Table.default (\msg -> RootMsg.TablesStoriesMsg (TablesMsg.Msg msg))
        |> Table.withHeader header
        |> Table.withBody body
