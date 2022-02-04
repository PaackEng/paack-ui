module Datepicker.Model exposing (..)

import Date exposing (Date)
import Datepicker.Msg exposing (Msg(..))
import Msg as RootMsg
import Time
import UI.DatePicker as DatePicker exposing (DatePicker)


type alias Model =
    { selected : Maybe Date
    , datepicker : DatePicker RootMsg.Msg
    }


initModel : Model
initModel =
    { selected = Nothing
    , datepicker = DatePicker.init Time.utc (Time.millisToPosix 1643359137775) Nothing (ToDatePicker >> RootMsg.DatepickerStoriesMsg)
    }
