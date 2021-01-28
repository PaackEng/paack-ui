module UI.Internal.Tables.Sorters exposing
    ( Msg
    , Sorter(..)
    , Sorters
    , SortingDirection(..)
    , itemsApplySorting
    , notSorting
    , sortBy
    , sortDecreasing
    , sortIncreasing
    , sortersEmpty
    , unsortable
    , update
    )

import UI.Internal.Basics exposing (flip, maybeNotThen)
import UI.Internal.NArray as NArray exposing (NArray)
import UI.Utils.TypeNumbers as T


type Msg
    = ToDo


type Sorter item
    = Sortable (item -> String)
    | Unsortable


type SortingDirection
    = SortIncreasing
    | SortDecreasing


type alias SortingStatus item =
    { column : Int, by : item -> String, direction : SortingDirection }


type Sorters item columns
    = Sorters
        { columns : NArray (Sorter item) columns
        , status : Maybe (SortingStatus item)
        }


update : Msg -> Sorters item columns -> ( Sorters item columns, Cmd msg )
update _ sorters =
    ( sorters, Cmd.none )


itemsApplySorting : Sorters item columns -> List item -> List item
itemsApplySorting (Sorters sorters) items =
    case sorters.status of
        Nothing ->
            items

        Just status ->
            let
                sortedItems =
                    List.sortBy status.by items
            in
            case status.direction of
                SortIncreasing ->
                    sortedItems

                SortDecreasing ->
                    List.reverse sortedItems


sortersEmpty : Sorters item T.Zero
sortersEmpty =
    Sorters
        { columns = NArray.empty
        , status = Nothing
        }


sortBy :
    (item -> String)
    -> Sorters item columns
    -> Sorters item (T.Increase columns)
sortBy fn (Sorters accu) =
    Sorters
        { columns = NArray.push (Sortable fn) accu.columns
        , status = accu.status
        }


unsortable : Sorters item columns -> Sorters item (T.Increase columns)
unsortable (Sorters accu) =
    Sorters
        { columns = NArray.push Unsortable accu.columns
        , status = accu.status
        }


sortIncreasing : Int -> Sorters item columns -> Sorters item columns
sortIncreasing =
    sort SortIncreasing


sortDecreasing : Int -> Sorters item columns -> Sorters item columns
sortDecreasing =
    sort SortDecreasing


sort : SortingDirection -> Int -> Sorters item columns -> Sorters item columns
sort direction column ((Sorters accu) as sorters) =
    case NArray.get column accu.columns of
        Just (Sortable by) ->
            Sorters
                { accu
                    | status =
                        Just
                            { column = column
                            , by = by
                            , direction = direction
                            }
                }

        Just Unsortable ->
            sorters

        Nothing ->
            sorters


notSorting : Sorters item columns -> Sorters item columns
notSorting (Sorters sorters) =
    Sorters { sorters | status = Nothing }
