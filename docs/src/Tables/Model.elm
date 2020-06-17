module Tables.Model exposing (Model, initModel)


type alias Model =
    { selected : Maybe String
    }


initModel : Model
initModel =
    { selected = Nothing
    }
