module I18n.English exposing (..)

import I18n.Type exposing (..)


filtersRange : FiltersRange
filtersRange =
    { after = "After"
    , before = "Before"
    , on = "On"
    }


filters : Filters
filters =
    { range = filtersRange
    }


root : Root
root =
    { filters = filters
    }
