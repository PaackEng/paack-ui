module Buttons.Model exposing (Model, initModel)

import UI.Table as Table


type alias Model =
    { demoSwitch : Bool
    }


initModel : Model
initModel =
    { demoSwitch = True
    }
