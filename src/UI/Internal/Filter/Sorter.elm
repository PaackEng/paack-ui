module UI.Internal.Filter.Sorter exposing (Sorter(..), SortingDirection(..), Status, preview, sort)


type Sorter item
    = AlphabeticalSortable (item -> String)
    | CharSortable (item -> Char)
    | CustomSortable (item -> item -> Order)
    | FloatSortable (item -> Float)
    | IntegerSortable (item -> Int)


type SortingDirection
    = SortAscending
    | SortDescending


type alias Status item =
    ( Maybe SortingDirection, Sorter item )


sort : Sorter item -> SortingDirection -> List item -> List item
sort sorter direction list =
    let
        listSortBy retrieve =
            case direction of
                SortAscending ->
                    List.sortBy retrieve

                SortDescending ->
                    List.sortWith (\a b -> compare (retrieve b) (retrieve a))

        listSortWith comparer =
            case direction of
                SortAscending ->
                    List.sortWith comparer

                SortDescending ->
                    List.sortWith (\a b -> comparer b a)
    in
    case sorter of
        AlphabeticalSortable retrieve ->
            listSortBy retrieve list

        CharSortable retrieve ->
            listSortBy retrieve list

        CustomSortable applier ->
            listSortWith applier list

        FloatSortable retrieve ->
            listSortBy retrieve list

        IntegerSortable retrieve ->
            listSortBy retrieve list


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
