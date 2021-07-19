module UI.Internal.Filter.Sorter exposing (Sorter(..), SortingDirection(..), preview, sort)


type Sorter item
    = AlphabeticalSortable (item -> String)
    | CharSortable (item -> Char)
    | CustomSortable (List item -> List item)
    | FloatSortable (item -> Float)
    | IntegerSortable (item -> Int)


type SortingDirection
    = SortAscending
    | SortDescending


sort : Sorter item -> List item -> List item
sort sorter list =
    case sorter of
        AlphabeticalSortable retrieve ->
            List.sortBy retrieve list

        CharSortable retrieve ->
            List.sortBy retrieve list

        CustomSortable applier ->
            applier list

        FloatSortable retrieve ->
            List.sortBy retrieve list

        IntegerSortable retrieve ->
            List.sortBy retrieve list


preview : Sorter item -> Maybe ( String, String )
preview sorter =
    case sorter of
        AlphabeticalSortable _ ->
            Just ( "A", "Z" )

        CharSortable _ ->
            Just ( "A", "Z" )

        CustomSortable _ ->
            Nothing

        FloatSortable _ ->
            Just ( "0", "9" )

        IntegerSortable _ ->
            Just ( "0", "9" )
