module Msg exposing (Msg(..))

import Form.State as Form
import Tables.Msg as Tables


type Msg
    = TablesStoriesMsg Tables.Msg
    | FormStoriesMsg Form.Msg
    | NoOp
