module Switches.Model exposing (Model, initModel)


type alias Model =
    { switch1 : Bool
    , switch2 : Bool
    , switch3 : Bool
    }


initModel : Model
initModel =
    { switch1 = False
    , switch2 = False
    , switch3 = True
    }
