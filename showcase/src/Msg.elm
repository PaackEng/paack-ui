module Msg exposing (Msg(..))

import Buttons.Msg as Buttons
import Paginators.Msg as Paginators
import Tables.Msg as Tables


type Msg
    = ButtonsStoriesMsg Buttons.Msg
    | PaginatorsStoriesMsg Paginators.Msg
    | TablesStoriesMsg Tables.Msg
    | NoOp
