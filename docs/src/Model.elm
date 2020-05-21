module Model exposing
    ( Model
    , init
    )

import Buttons.Model as Buttons
import Form.State as FormState


type alias Model =
    { formStories : FormState.Model
    , buttonsStories : Buttons.Model
    }


init : Model
init =
    { formStories = FormState.initModel
    , buttonsStories = Buttons.initModel
    }
