module UI.I18n.English exposing (..)

import UI.I18n.Types exposing (..)


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


dropdown : Dropdown
dropdown =
    { show = "Expand"
    , collapse = "Collapse"
    }


listView : ListView
listView =
    { search = "Search"
    , selectAll = "Select All"
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


tablesSorting : TablesSorting
tablesSorting =
    { increase = "Sort from A - Z"
    , decrease = "Sort from Z - A"
    }


tables : Tables
tables =
    { details = tablesDetails
    , sorting = tablesSorting
    , selectRow = "Select this row."
    , selectAll = "Select all rows"
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
    , moreActions = "More actions"
    }


contentPlaceholdersNothingToSeeHere : ContentPlaceholdersNothingToSeeHere
contentPlaceholdersNothingToSeeHere =
    { title = "Nothing to see here"
    , body = "Look, you tried looking here. It’s ok, life can be difficult at times when we don’t find what we’re looking for..."
    }


contentPlaceholders : ContentPlaceholders
contentPlaceholders =
    { nothingToSeeHere = contentPlaceholdersNothingToSeeHere
    }


root : Root
root =
    { filters = filters
    , paginator = paginator
    , checkbox = checkbox
    , dropdown = dropdown
    , listView = listView
    , radio = radio
    , tables = tables
    , dateInput = dateInput
    , dialog = dialog
    , sidebar = sidebar
    , contentPlaceholders = contentPlaceholders
    }
