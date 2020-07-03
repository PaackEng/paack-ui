module Tables.Msg exposing (Msg(..))

import Tables.Model exposing (..)
import UI.Table as Table


type Msg
    = ForComponent Table.Msg
