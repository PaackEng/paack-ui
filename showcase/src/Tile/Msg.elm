module Tile.Msg exposing (Msg(..))

import Browser.Dom as Dom
import Tile.Model as Model


type Msg
    = Select Model.Options
