module I18n.Spanish exposing (..)

import I18n.Types exposing (..)


common : Common
common =
    { dateFormat = "DD/MM/YYYY"
    , close = "Close"
    , clear = "Clear"
    , apply = "Apply"
    , previous = "Previous"
    , next = "Next"
    , toggle = "Toggle"
    }


filtersPeriod : FiltersPeriod
filtersPeriod =
    { after = "After"
    , before = "Before"
    , on = "On"
    , description = "Select period reference"
    }


filtersRange : FiltersRange
filtersRange =
    { from = \{ date } -> "From: " ++ date ++ ""
    , to = \{ date } -> "To: " ++ date ++ ""
    }


filtersSelect : FiltersSelect
filtersSelect =
    { description = "Select option for filtering"
    }


filters : Filters
filters =
    { period = filtersPeriod
    , range = filtersRange
    , select = filtersSelect
    }


paginator : Paginator
paginator =
    { format = \{ first, last, total } -> "" ++ first ++ " - " ++ last ++ " of " ++ total ++ ""
    }


root : Root
root =
    { common = common
    , filters = filters
    , paginator = paginator
    }
