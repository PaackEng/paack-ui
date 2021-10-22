module Tile.Model exposing (Model, Options(..), initModel)


type alias Model =
    { selected : Maybe Options
    }


type Options
    = Car
    | Van
    | Truck
    | Motorbike
    | Bicycle
    | ECar
    | EVan
    | EBike


initModel : Model
initModel =
    { selected = Just ECar
    }
