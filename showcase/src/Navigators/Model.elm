module Navigators.Model exposing (Model, initModel)

import UI.NavigationContainer as Nav
import UI.RenderConfig exposing (RenderConfig)

type alias Model =
    { state : Nav.State
    }


initModel : RenderConfig -> Model
initModel renderConfig =
    { state = Nav.stateInit renderConfig
    }
