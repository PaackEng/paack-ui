module Msg exposing (Msg(..))

import Buttons.Msg as Buttons
import Paginators.Msg as Paginators


type Msg
    = ButtonsStoriesMsg Buttons.Msg
    | PaginatorsStoriesMsg Paginators.Msg
    | NoOp
