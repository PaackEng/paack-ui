module Msg exposing (Msg(..))

import Buttons.Msg as Buttons
import Form.State as Form
import Paginators.Msg as Paginators


type Msg
    = FormStoriesMsg Form.Msg
    | ButtonsStoriesMsg Buttons.Msg
    | PaginatorsStoriesMsg Paginators.Msg
    | NoOp
