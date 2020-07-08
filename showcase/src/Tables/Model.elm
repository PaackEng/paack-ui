module Tables.Model exposing (..)

import Msg exposing (Msg)
import UI.Tables.Stateful as Table exposing (filtersEmpty, localSingleTextFilter)
import UI.Utils.TypeNumbers exposing (Three)


type alias Model =
    { tableState : Table.State Msg Book Three }


type alias Book =
    { author : String
    , title : String
    , year : String
    }


initModel : Model
initModel =
    { tableState =
        Table.stateWithFilters someFilters Table.init
    }


someFilters =
    filtersEmpty
        |> localSingleTextFilter Nothing .title
        |> localSingleTextFilter (Just "Dan") .author
        |> localSingleTextFilter Nothing .year
