module DatePicker.Msg exposing (..)

import Date exposing (Date)
import UI.DatePicker as DatePicker


type Msg
    = ToDatePicker DatePicker.Msg
    | Select Date
