module UI.I18n.Types exposing (..)


type alias FiltersPeriod =
    { after : String
    , before : String
    , on : String
    , description : String
    }


type alias FiltersRange =
    { from : { date : String } -> String
    , to : { date : String } -> String
    }


type alias FiltersSelect =
    { description : String
    }


type alias Filters =
    { dateFormat : String
    , close : String
    , clear : String
    , apply : String
    , period : FiltersPeriod
    , range : FiltersRange
    , select : FiltersSelect
    }


type alias Paginator =
    { format : { first : String, last : String, total : String } -> String
    , previous : String
    , next : String
    , tableFormat : { first : String, last : String, total : String } -> String
    , first : String
    , last : String
    , rowsPerPage : String
    }


type alias Checkbox =
    { toggle : String
    }


type alias ListView =
    { search : String
    , selectAll : String
    }


type alias Radio =
    { select : String
    }


type alias TablesDetails =
    { show : String
    , collapse : String
    }


type alias TablesSorting =
    { ascending : String
    , descending : String
    }


type alias Tables =
    { details : TablesDetails
    , sorting : TablesSorting
    , selectRow : String
    , selectAll : String
    }


type alias DateInput =
    { invalid : String
    }


type alias Dialog =
    { close : String
    }


type alias Sidebar =
    { expand : String
    , collapse : String
    , previous : String
    , moreActions : String
    }


type alias ContentPlaceholdersNothingToSeeHere =
    { title : String
    , body : String
    }


type alias ContentPlaceholders =
    { nothingToSeeHere : ContentPlaceholdersNothingToSeeHere
    }


type alias Dropdown =
    { show : String
    , collapse : String
    }


type alias Calendar =
    { jan : String
    , feb : String
    , mar : String
    , apr : String
    , may : String
    , jun : String
    , jul : String
    , aug : String
    , sep : String
    , oct : String
    , nov : String
    , dec : String
    , mon : String
    , tue : String
    , wed : String
    , thu : String
    , fri : String
    , sat : String
    , sun : String
    , prevMonth : String
    , nextMonth : String
    }


type alias Root =
    { filters : Filters
    , paginator : Paginator
    , checkbox : Checkbox
    , listView : ListView
    , radio : Radio
    , tables : Tables
    , dateInput : DateInput
    , dialog : Dialog
    , sidebar : Sidebar
    , contentPlaceholders : ContentPlaceholders
    , dropdown : Dropdown
    , calendar : Calendar
    }
