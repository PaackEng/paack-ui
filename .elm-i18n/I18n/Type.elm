module I18n.Type exposing (..)


type alias FiltersRange =
    { after : String
    , before : String
    , on : String
    }


type alias Filters =
    { range : FiltersRange
    }


type alias Root =
    { filters : Filters
    }
