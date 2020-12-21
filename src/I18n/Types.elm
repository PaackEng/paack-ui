module I18n.Types exposing (Checkbox, DateInput, Dialog, Filters, FiltersPeriod, FiltersRange, FiltersSelect, ListView, Paginator, Radio, Root, Sidebar, Tables, TablesDetails)


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


type alias Tables =
    { details : TablesDetails
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
    }
