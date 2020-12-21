module I18n.English exposing (root)

import I18n.Types exposing (..)


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
    { dateFormat = "DD/MM/YYYY"
    , close = "Close"
    , clear = "Clear"
    , apply = "Apply"
    , period = filtersPeriod
    , range = filtersRange
    , select = filtersSelect
    }


paginator : Paginator
paginator =
    { format = \{ first, last, total } -> "" ++ first ++ " - " ++ last ++ " of " ++ total ++ ""
    , previous = "Previous"
    , next = "Next"
    }


checkbox : Checkbox
checkbox =
    { toggle = "Toggle"
    }


listView : ListView
listView =
    { search = "Search"
    }


radio : Radio
radio =
    { select = "Select item"
    }


tablesDetails : TablesDetails
tablesDetails =
    { show = "Expand"
    , collapse = "Collapse"
    }


tables : Tables
tables =
    { details = tablesDetails
    }


dateInput : DateInput
dateInput =
    { invalid = "Invalid date format."
    }


dialog : Dialog
dialog =
    { close = "Close dialog"
    }


sidebar : Sidebar
sidebar =
    { expand = "Expand sidebar"
    , collapse = "Minimize sidebar"
    , previous = "Go back"
    }


root : Root
root =
    { filters = filters
    , paginator = paginator
    , checkbox = checkbox
    , listView = listView
    , radio = radio
    , tables = tables
    , dateInput = dateInput
    , dialog = dialog
    , sidebar = sidebar
    }
