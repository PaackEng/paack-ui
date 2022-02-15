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
    , tableFormat = \{ first, last, total } -> "" ++ first ++ "-" ++ last ++ " of " ++ total ++ ""
    , previous = "Previous"
    , next = "Next"
    , first = "First"
    , last = "Last"
    , rowsPerPage = "Rows/Page"
    }


checkbox : Checkbox
checkbox =
    { toggle = "Toggle"
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
    { ascending = "Sort Ascending"
    , descending = "Sort Descending"
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


dropdown : Dropdown
dropdown =
    { show = "Expand"
    , collapse = "Collapse"
    }


calendar : Calendar
calendar =
    { jan = "January"
    , feb = "February"
    , mar = "March"
    , apr = "April"
    , may = "May"
    , jun = "June"
    , jul = "July"
    , aug = "August"
    , sep = "September"
    , oct = "October"
    , nov = "November"
    , dec = "December"
    , mon = "Mon"
    , tue = "Tue"
    , wed = "Wed"
    , thu = "Thu"
    , fri = "Fri"
    , sat = "Sat"
    , sun = "Sun"
    , prevMonth = "Previous Month"
    , nextMonth = "Next Month"
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
    , contentPlaceholders = contentPlaceholders
    , dropdown = dropdown
    , calendar = calendar
    }
