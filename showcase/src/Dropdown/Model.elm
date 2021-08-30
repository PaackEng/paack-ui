module Dropdown.Model exposing (Model, initModel)

import Tables.Book exposing (Book)
import UI.Dropdown as Dropdown


type alias Model =
    { dropdownState : Dropdown.State Book
    , selectedBook : Maybe Book
    , filterText : String
    }


initModel : Model
initModel =
    { dropdownState = Dropdown.init "default-dropdown"
    , selectedBook = Nothing
    , filterText = ""
    }
