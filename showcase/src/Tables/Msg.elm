module Tables.Msg exposing (Msg(..))

import Tables.Model exposing (..)
import UI.Tables.Stateable as Table


type Msg
    = ForComponent Table.Msg
