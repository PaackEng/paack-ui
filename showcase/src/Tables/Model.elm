module Tables.Model exposing (Model, initModel)

import Msg exposing (Msg)
import Tables.Book as Book exposing (Book)
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
            |> Table.stateWithFilters Book.filters
            |> Table.stateWithSorters Book.sorters
            |> Table.stateWithItems
                (List.concatMap
                    (always Book.books)
                    (List.range 1 30)
                )
            |> Table.stateWithPaginator
    , selecTableState =
        Table.init
            |> Table.stateWithSelection .isbn False
            |> Table.stateWithFilters Book.filters
            |> Table.stateWithSorters Book.sorters
            |> Table.stateWithItems Book.books
    }
