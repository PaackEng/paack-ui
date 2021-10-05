module UI.Utils.DateInput exposing
    ( DateInput, PeriodComparison
    , PeriodDate, RangeDate
    , toDD_MM_YYYY, fromPosix
    )

{-| Required type for comparing and validating date inputs.


# Internal types

@docs DateInput, PeriodComparison


# Useful record

@docs PeriodDate, RangeDate


# Transformation

@docs toDD_MM_YYYY, fromPosix

-}

import Time
import UI.Internal.DateInput as Internal


{-| `DateInput.DateInput` is used for describing dates in the UI.
-}
type alias DateInput =
    Internal.DateInput


{-| `DateInput.PeriodComparison` is used to compare date and time ranges.
-}
type alias PeriodComparison =
    Internal.PeriodComparison


{-| `DateInput.PeriodDate` is used to contain both date and comparison information.
-}
type alias PeriodDate =
    { date : DateInput, comparison : PeriodComparison }


{-| `DateInput.RangeDate` is used to describe a range of dates.
-}
type alias RangeDate =
    { from : DateInput, to : DateInput }


{-| Transforms a string into a `DateInput.DateInput`
-}
toDD_MM_YYYY : String -> DateInput -> String
toDD_MM_YYYY =
    Internal.toDD_MM_YYYY


{-| Transform a time with time zone into a DateInput
-}
fromPosix : Time.Zone -> Time.Posix -> DateInput
fromPosix =
    Internal.fromPosix
