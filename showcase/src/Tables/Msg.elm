module Tables.Msg exposing (Msg(..))

import Tables.Book exposing (..)
import UI.Tables.Stateful as Table


type Msg
    = ForMain (Table.Msg Book)
    | ForSelectable (Table.Msg Book)
