module Msg exposing (Msg(..))

import Buttons.Msg as Buttons
import Form.State as Form


type Msg
    = FormStoriesMsg Form.Msg
    | ButtonsStoriesMsg Buttons.Msg
    | NoOp
