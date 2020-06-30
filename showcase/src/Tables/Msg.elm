module Tables.Msg exposing (Msg(..))

import Tables.Model exposing (..)


type Msg
    = Select String
    | FilterApply
    | FilterDiscard
    | FilterClear Column
    | FilterEdit Column (Maybe String)
