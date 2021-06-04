module Layouts.Model exposing (Model, initModel)

import Tables.Book exposing (Book)


type alias Model =
    { selected : Maybe Book
    , filter : Maybe String
    , showExtraMenu : Bool
    }


initModel : Model
initModel =
    { selected = Nothing
    , filter = Nothing
    , showExtraMenu = False
    }
