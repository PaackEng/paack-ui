module Tables.Msg exposing (Msg(..))

import Tables.Model exposing (Player)
import UI.Table as Table


type Msg
    = Msg (Table.Msg Player)
