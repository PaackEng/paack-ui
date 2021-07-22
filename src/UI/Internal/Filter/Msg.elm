module UI.Internal.Filter.Msg exposing (Msg(..))

import Browser.Dom as Dom
import UI.Internal.DateInput exposing (PeriodComparison)


type Msg
    = EditSingleText String
    | EditMultiText Int String
    | EditSingleDate String
    | EditRangeFromDate String
    | EditRangeToDate String
    | EditPeriodDate String
    | EditPeriodComparison String PeriodComparison
    | EditSelect String Int
    | DomFocusResult (Result Dom.Error ())
    | Apply
    | Clear
