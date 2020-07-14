module Tables.Model exposing (..)

import Msg exposing (Msg)
import Time
import UI.Tables.Stateful as Table
import UI.Utils.TypeNumbers exposing (Five)


type alias Model =
    { tableState : Table.State Msg Book Five }


type alias Book =
    { author : String
    , title : String
    , year : String
    , acquired : Time.Posix
    , read : Time.Posix
    }


initModel : Model
initModel =
    { tableState =
        Table.stateWithFilters someFilters Table.init
    }


someFilters =
    Table.filtersEmpty
        |> Table.localMultiTextFilter [] .title
        |> Table.localMultiTextFilter [ "Dan" ] .author
        |> Table.localSelectFilter
            [ "Last Decade", "New Millennium", "Old Century" ]
            Nothing
            (\item selected ->
                case selected of
                    0 ->
                        String.startsWith "201" item.year

                    1 ->
                        String.startsWith "20" item.year

                    2 ->
                        String.startsWith "19" item.year

                    _ ->
                        False
            )
        |> Table.localRangeDateFilter Time.utc Nothing Nothing .acquired
        |> Table.localPeriodDateFilter Time.utc Nothing Nothing .read
