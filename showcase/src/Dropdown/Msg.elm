module Dropdown.Msg exposing (Msg(..))

import Tables.Book exposing (Book)
import UI.Dropdown as Dropdown


type Msg
    = ForDropdownMsg (Dropdown.Msg Book)
    | SelectMsg (Maybe Book)
