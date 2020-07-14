module Model exposing
    ( Model
    , init
    )

import Buttons.Model as Buttons
import Checkboxes.Model as Checkboxes
import Paginators.Model as Paginators
import Tables.Model as Tables


type alias Model =
    { buttonsStories : Buttons.Model
    , paginatorsStories : Paginators.Model
    , tablesStories : Tables.Model
    , checkboxesStories : Checkboxes.Model
    }


init : Model
init =
    { buttonsStories = Buttons.initModel
    , paginatorsStories = Paginators.initModel
    , tablesStories = Tables.initModel
    , checkboxesStories = Checkboxes.initModel
    }
