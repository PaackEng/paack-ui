module Model exposing
    ( Model
    , init
    )

import Buttons.Model as Buttons
import Form.State as FormState
import Tables.Model as Tables


type alias Model =
    { tablesStories : Tables.Model
    , formStories : FormState.Model
    , buttonsStories : Buttons.Model
    }


init : Model
init =
    { tablesStories = Tables.initModel
    , formStories = FormState.initModel
    , buttonsStories = Buttons.initModel
    }
