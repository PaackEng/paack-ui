module UI.Internal.Tables.Sorters exposing (Sorter(..), Sorters, SortingStatus(..), notSorting, sortBy, sortDecreasing, sortIncreasing, unsortable)

import UI.Internal.Basics exposing (flip, maybeNotThen)
import UI.Internal.NArray as NArray exposing (NArray)
import UI.Utils.TypeNumbers as T


type Sorter item
    = Sortable
        { sortBy : item -> String
        , status : SortingStatus
        }
    | Unsortable


type SortingStatus
    = NotSorting
    | SortIncreasing
    | SortDecreasing


type alias Sorters item columns =
    { columns : NArray (Filter item) columns
    , activeColumn : Maybe { activeIndex : Int, cachedIndex : List String }
    }


sortersEmpty =
    ()


sortBy :
    (item -> String)
    -> SortingStatus
    -> Sorters item columns
    -> Sorters item (T.Increase columns)
sortBy fn status =
    { sortBy = sortBy
    , status = status
    }
        |> Sortable
        |> flip NArray.push accu


unsortable =
    ()


sortIncreasing =
    ()


sortDecreasing =
    ()


notSorting =
    ()
