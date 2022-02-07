module DatePicker.Model exposing (..)

import Date exposing (Date)
import DatePicker.Msg exposing (Msg(..))
import Msg as RootMsg
import Time
import UI.DatePicker as DatePicker


type alias Model =
    { selected : Maybe Date
    , datePicker : DatePicker.Model
    }


initModel : Model
initModel =
    { selected = Nothing
    , datePicker = DatePicker.init (Date.fromCalendarDate 2022 Time.Jan 1)
    }
