module Filters.Model exposing (Model, initModel)

import Msg exposing (Msg)
import UI.Filter as Filter exposing (FilterModel)


type alias Model =
    { demoFilter : FilterModel Msg String
    }


initModel : Model
initModel =
    { demoFilter =
        Filter.multiTextFilter [] identity
            |> Filter.setItems [ "Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune" ]
    }
