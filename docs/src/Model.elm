module Model exposing
    ( Model
    , init
    )

import Buttons.Model as Buttons
import Form.State as FormState
import Paginators.Model as Paginators


type alias Model =
    { formStories : FormState.Model
    , buttonsStories : Buttons.Model
    , paginatorsStories : Paginators.Model
    }


init : Model
init =
    { formStories = FormState.initModel
    , buttonsStories = Buttons.initModel
    , paginatorsStories = Paginators.initModel
    }
