module Tables.Msg exposing (Msg(..))

import Tables.Model exposing (..)
import UI.Tables.Stateful as Table


type Msg
    = ForComponent Table.Msg
