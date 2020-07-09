module Tables.Model exposing (..)

import Msg exposing (Msg)
import UI.Tables.Stateful as Table exposing (filtersEmpty, localSelectFilter, localSingleTextFilter)
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
        |> localSelectFilter
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
