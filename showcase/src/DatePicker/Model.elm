module DatePicker.Model exposing (..)

import Calendar exposing (Date)
import Time
import UI.DatePicker as DatePicker


type alias Model =
    { selected : Maybe Date
    , datePicker : DatePicker.Model
    }


initModel : Model
initModel =
    { selected = Nothing
    , datePicker = DatePicker.init <| Calendar.fromPosix <| Time.millisToPosix 1641016800000
    }
