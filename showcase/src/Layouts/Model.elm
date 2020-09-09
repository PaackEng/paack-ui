module Layouts.Model exposing (Model, initModel)


type alias Model =
    { email : String, password : String }


initModel : Model
initModel =
    { email = "", password = "" }
