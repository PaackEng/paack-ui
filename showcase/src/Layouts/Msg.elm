module Layouts.Msg exposing (Msg(..))

import Tables.Book exposing (Book)

type Msg
    = NoOp
    | Select Book
