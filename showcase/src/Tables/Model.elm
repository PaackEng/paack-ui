module Tables.Model exposing (..)

import Msg exposing (Msg)
import Tables.Book exposing (..)
import UI.Tables.Stateful as Table
import UI.Utils.TypeNumbers exposing (Five)


type alias Model =
    { mainTableState : Table.State Msg Book Five
    , selecTableState : Table.State Msg Book Five -- This name is an IT joke
    }


initModel : Model
initModel =
    { mainTableState =
        Table.init
            |> Table.stateWithFilters someFilters
    , selecTableState =
        Table.init
            |> Table.stateWithSelection .isbn False
            |> Table.stateWithFilters someFilters
    }
