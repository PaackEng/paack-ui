module Menu.Model exposing (Model, initModel)


type alias Model =
    { isVisible : Bool
    }


initModel : Model
initModel =
    { isVisible = False
    }
