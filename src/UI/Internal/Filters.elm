module UI.Internal.Filters exposing (..)

import Dict exposing (Dict)
import UI.Internal.NArray as NArray exposing (NArray)


type alias Filters msg item columns =
    NArray (Filter msg item) columns


type Filter msg item
    = SingleTextFilter (SingleTextFilterConfig msg item)
    | MultiTextFilter (MultiTextFilterConfig msg item)
    | SingleDateFilter (SingleDateFilterConfig msg item)
    | RangeDateFilter (RangeDateFilterConfig msg item)
    | PeriodDateFilter (PeriodDateFilterConfig msg item)
    | SelectFilter (SelectFilterConfig msg item)


type FilterModel
    = SingleTextFilter (Editable String)
    | MultiTextFilter (Dict Int (Editable String))
    | SingleDateFilter (Editable String)
    | RangeDateFilter { from : Editable String, to : Editable String }
    | PeriodDateFilter { date : Editable String, timePeriod : Editable TimePeriod }
    | SelectFilter (Editable Int)


type alias Editable something =
    { current : Maybe something, applied : Maybe something }


type TimePeriod
    = On
    | Before
    | After


type Strategy msgs value item
    = Local (value -> item -> Bool)
    | Remote msgs



-- SingleText


type alias SingleTextFilterConfig msg item =
    { init : String
    , strategy : SingleTextFilterStrategy msg item
    }


type alias SingleTextFilterStrategy msg item =
    Strategy (SingleTextFilterRemote msg) String item


type alias SingleTextFilterRemote msg =
    { editMsg : String -> msg
    }



-- MultiText


type alias MultiTextFilterConfig msg item =
    { init : List String
    , strategy : MultiTextFilterStrategy msg item
    }


type alias MultiTextFilterStrategy msg item =
    Strategy (MultiTextFilterRemote msg) (List String) item


type alias MultiTextFilterRemote msg =
    { editMsg : Int -> String -> msg
    }



-- SingleDate


type alias Date =
    String


type alias SingleDateFilterConfig msg item =
    { init : Maybe Date
    , strategy : SingleDateFilterStrategy msg item
    }


type alias SingleDateFilterStrategy msg item =
    Strategy (SingleDateFilterRemote msg) Date item


type alias SingleDateFilterRemote msg =
    { editMsg : Date -> msg
    }



-- RangeDate


type alias RangeDate =
    { from : Date, to : String }


type alias RangeDateFilterConfig msg item =
    { init : Maybe Date
    , strategy : RangeDateFilterStrategy msg item
    }


type alias RangeDateFilterStrategy msg item =
    Strategy (RangeDateFilterRemote msg) RangeDate item


type alias RangeDateFilterRemote msg =
    { fromEditMsg : Date -> msg
    , toEditMsg : String -> msg
    }



-- PeriodDate


type alias PeriodDate =
    { date : Date, timePeriod : TimePeriod }


type alias PeriodDateFilterConfig msg item =
    { init : PeriodDate
    , strategy : PeriodDateFilterStrategy msg item
    }


type alias PeriodDateFilterStrategy msg item =
    Strategy (PeriodDateFilterRemote msg) PeriodDate item


type alias PeriodDateFilterRemote msg =
    { dateEditMsg : String -> msg
    , periodEditMsg : TimePeriod -> msg
    }



-- SelectFilter


type alias SingleTextFilterConfig msg item =
    { init : Maybe Int
    , strategy : SingleTextFilterStrategy msg item
    }


type alias SingleTextFilterStrategy msg item =
    Strategy (SingleTextFilterRemote msg) Int item


type alias SingleTextFilterRemote msg =
    { selectMsg : Int -> msg
    }
