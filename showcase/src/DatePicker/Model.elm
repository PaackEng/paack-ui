module DatePicker.Model exposing (..)

import Calendar exposing (Date)
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
    , datePicker = DatePicker.init <| Calendar.fromPosix <| Time.millisToPosix 1644248723983
    }
