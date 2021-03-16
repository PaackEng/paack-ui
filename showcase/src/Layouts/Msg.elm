module Layouts.Msg exposing (Msg(..))

import Tables.Book exposing (Book)


type Msg
    = Select Book
    | Filter String
