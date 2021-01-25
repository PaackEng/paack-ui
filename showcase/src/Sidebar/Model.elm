module Sidebar.Model exposing (Model, initModel)

import Tables.Book exposing (Book)


type alias Model =
    { expanded : Bool }


initModel : Model
initModel =
    { expanded = False }
