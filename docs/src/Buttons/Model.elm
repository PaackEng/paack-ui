module Buttons.Model exposing (Model, initModel)


type alias Model =
    { demoSwitch : Bool
    }


initModel : Model
initModel =
    { demoSwitch = True
    }
