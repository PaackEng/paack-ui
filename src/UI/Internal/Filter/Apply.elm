module UI.Internal.Filter.Apply exposing (apply)

import UI.Internal.Filter.Model as Filter exposing (Filter)
import UI.Internal.Filter.Sorter as Sorter


apply : Filter msg item -> Maybe (Sorter.Status item) -> List item -> List item
apply filter sorting items =
    let
        filteredResults =
            case Filter.filterGet filter of
                Just filterFunction ->
                    List.filter filterFunction items

                Nothing ->
                    items
    in
    case sorting of
        Just ( Just direction, sorter ) ->
            Sorter.sort sorter direction filteredResults

        _ ->
            filteredResults
