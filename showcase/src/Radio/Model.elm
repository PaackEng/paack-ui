module Radio.Model exposing (Model, Options(..), initModel)


type alias Model =
    { selected : Maybe Options
    }


type Options
    = Queen
    | Beatles -- Really overrated
    | ACDC
    | LedZeppelin
    | PinkFloyd


initModel : Model
initModel =
    { selected = Nothing
    }
