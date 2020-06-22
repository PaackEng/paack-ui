module Paginators.Model exposing (Model, initModel)


type alias Model =
    { items : List String
    , offset : Int
    }


initModel : Model
initModel =
    { items =
        List.range 1 60
            |> List.map (\i -> "Item " ++ String.fromInt i)
    , offset = 0
    }
