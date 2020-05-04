module Msg exposing (Msg(..))

import Buttons.Msg as Buttons
import Form.State as Form
import Tables.Msg as Tables


type Msg
    = TablesStoriesMsg Tables.Msg
    | FormStoriesMsg Form.Msg
    | ButtonsStoriesMsg Buttons.Msg
    | NoOp
