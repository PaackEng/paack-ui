module UI.Internal.Filter.Sorter exposing (..)


type Sorter item
    = AlphabeticalSortable (item -> String)


type SortingDirection
    = SortAscending
    | SortDescending


type alias AlphanumericSortConfig item =
    { retrieve : item -> String
    , smallerFirst : Bool
    , emptyOnTail : Bool
    , numbersFirst : Bool
    }


alphanumericSort : AlphanumericSortConfig item -> List item -> List item
alphanumericSort { retrieve, smallerFirst, emptyOnTail, numbersFirst } list =
    let
        nonEmptySort nonEmptyList =
            if smallerFirst then
                List.sortWith
                    (\a b -> compareSmallerFirst (retrieve a) (retrieve b))
                    nonEmptyList

            else
                List.sortBy retrieve nonEmptyList

        alphanumericMerge ( numbers, alphanums, empties ) =
            let
                sortedNumbers =
                    numbers
                        |> List.sortBy Tuple.first
                        |> List.map Tuple.second

                sortedAlphanums =
                    nonEmptySort alphanums
            in
            if emptyOnTail then
                sortedNumbers ++ sortedAlphanums ++ empties

            else
                empties ++ sortedNumbers ++ sortedAlphanums
    in
    if numbersFirst then
        List.foldr
            (alphanumericPartition retrieve)
            ( [], [], [] )
            list
            |> alphanumericMerge

    else if emptyOnTail then
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


alphanumericPartition :
    (item -> String)
    -> item
    -> ( List ( Float, item ), List item, List item )
    -> ( List ( Float, item ), List item, List item )
alphanumericPartition retrieve elem ( numbers, alphanums, empties ) =
    let
        string =
            retrieve elem
    in
    if String.isEmpty string then
        ( numbers, alphanums, elem :: empties )

    else
        case String.toFloat string of
            Just float ->
                ( ( float, elem ) :: numbers, alphanums, empties )

            Nothing ->
                ( numbers, elem :: alphanums, empties )


sort : Sorter item -> List item -> List item
sort sorter list =
    case sorter of
        AlphabeticalSortable retrieve ->
            List.sortBy retrieve list
