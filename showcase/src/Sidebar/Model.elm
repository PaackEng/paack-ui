module Sidebar.Model exposing (Model, initModel)


type alias Model =
    { expanded : Bool }


initModel : Model
initModel =
    { expanded = False }
