module Checkboxes.Model exposing (Model, initModel)


type alias Model =
    { box1 : Bool
    , box2 : Bool
    , box3 : Bool
    }


initModel : Model
initModel =
    { box1 = False
    , box2 = False
    , box3 = True
    }
