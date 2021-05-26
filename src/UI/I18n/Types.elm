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
    }


type alias Checkbox =
    { toggle : String
    }


type alias ListView =
    { search : String
    }


type alias Radio =
    { select : String
    }


type alias TablesDetails =
    { show : String
    , collapse : String
    }


type alias TablesSorting =
    { increase : String
    , decrease : String
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
    }
