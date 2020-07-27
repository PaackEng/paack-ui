module I18n.Type exposing (..)


type alias Common =
    { dateFormat : String
    , close : String
    , clear : String
    , apply : String
    , previous : String
    , next : String
    }


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
    { period : FiltersPeriod
    , range : FiltersRange
    , select : FiltersSelect
    }


type alias Paginator =
    { format : { first : String, last : String, total : String } -> String
    }


type alias Root =
    { common : Common
    , filters : Filters
    , paginator : Paginator
    }
