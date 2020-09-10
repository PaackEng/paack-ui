module Layouts.Model exposing (Model, initModel)

import Tables.Book exposing (Book)

type alias Model =
    { selected : Maybe Book }


initModel : Model
initModel =
    { selected = Nothing }
