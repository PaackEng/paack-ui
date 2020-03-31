module Model exposing
    ( Model
    , init
    )

import Form.State as FormState
import Tables.Model as Tables


type alias Model =
    { tablesStories : Tables.Model
    , formStories : FormState.Model
    }


init : Model
init =
    { tablesStories = Tables.initModel
    , formStories = FormState.initModel
    }
