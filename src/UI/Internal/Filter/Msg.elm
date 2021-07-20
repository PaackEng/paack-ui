module UI.Internal.Filter.Msg exposing (Msg(..))

import UI.Internal.DateInput exposing (PeriodComparison)


type Msg
    = EditSingleText String
    | EditMultiText Int String
    | EditSingleDate String
    | EditRangeFromDate String
    | EditRangeToDate String
    | EditPeriodDate String
    | EditPeriodComparison PeriodComparison
    | EditSelect Int
    | Apply
    | Clear
