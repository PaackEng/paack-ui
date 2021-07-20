module UI.Internal.Filter.Msg exposing (Msg(..))

import UI.Internal.DateInput exposing (PeriodComparison)
import UI.Internal.Filter.Sorter exposing (SortingDirection)


type Msg
    = EditSingleText String
    | EditMultiText Int String
    | EditSingleDate String
    | EditRangeFromDate String
    | EditRangeToDate String
    | EditPeriodDate String
    | EditPeriodComparison PeriodComparison
    | EditSelect Int
    | SetSorting (Maybe SortingDirection)
    | Apply
    | Clear
