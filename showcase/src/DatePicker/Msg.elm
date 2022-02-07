module DatePicker.Msg exposing (..)

import Calendar exposing (Date)
import UI.DatePicker as DatePicker


type Msg
    = ToDatePicker DatePicker.Msg
    | Select Date
