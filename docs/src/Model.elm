module Model exposing
    ( Model
    , init
    )

import Buttons.Model as Buttons
import Paginators.Model as Paginators


type alias Model =
    { buttonsStories : Buttons.Model
    , paginatorsStories : Paginators.Model
    }


init : Model
init =
    { buttonsStories = Buttons.initModel
    , paginatorsStories = Paginators.initModel
    }
