module UI.Internal.Filter.Sorter exposing (..)


type Sorter item
    = AlphanumericSortable (AlphanumericSortConfig item)
    | IntegerSortable (item -> Int)
    | FloatingSortable (item -> Float)
    | CharSortable (item -> Float)
    | CustomSortable (item -> item -> Order)


type SortingDirection
    = SortIncreasing
    | SortDecreasing


type alias AlphanumericSortConfig item =
    { retrieve : item -> String
    , smallerFirst : Bool
    , emptyOnTail : Bool
    }


alphanumericSort : AlphanumericSortConfig item -> List item -> List item
alphanumericSort { retrieve, smallerFirst, emptyOnTail } list =
    let
        nonEmptySort nonEmptyList =
            if smallerFirst then
                List.sortWith
                    (\a b -> compareSmallerFirst (retrieve a) (retrieve b))
                    nonEmptyList

            else
                List.sortBy retrieve nonEmptyList
    in
    if emptyOnTail then
        List.partition (retrieve >> String.isEmpty)
            list
            |> Tuple.mapSecond nonEmptySort
            |> (\( a, b ) -> b ++ a)

    else
        nonEmptySort list


compareSmallerFirst : String -> String -> Order
compareSmallerFirst a b =
    case compare (String.length a) (String.length b) of
        EQ ->
            compare a b

        another ->
            another


sort : Sorter item -> List item -> List item
sort sorter list =
    case sorter of
        AlphanumericSortable config ->
            alphanumericSort config list

        IntegerSortable retrieve ->
            List.sortBy retrieve list

        FloatingSortable retrieve ->
            List.sortBy retrieve list

        CharSortable retrieve ->
            List.sortBy retrieve list

        CustomSortable fn ->
            List.sortWith fn list
