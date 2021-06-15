module ListView.Msg exposing (Msg(..))

import Tables.Book exposing (Book)


type Msg
    = SelectElement Book
    | FilterSet
    | None
