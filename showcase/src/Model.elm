module Model exposing
    ( Model
    , init
    )

import Buttons.Model as Buttons
import Checkboxes.Model as Checkboxes
import Paginators.Model as Paginators
import Radio.Model as Radio
import Tables.Model as Tables
import UIExplorer.Plugins.Tabs as TabsPlugin


type alias Model =
    { buttonsStories : Buttons.Model
    , paginatorsStories : Paginators.Model
    , tablesStories : Tables.Model
    , checkboxesStories : Checkboxes.Model
    , radioStories : Radio.Model
    , tabs : TabsPlugin.Model
    }


init : Model
init =
    { buttonsStories = Buttons.initModel
    , paginatorsStories = Paginators.initModel
    , tablesStories = Tables.initModel
    , checkboxesStories = Checkboxes.initModel
    , radioStories = Radio.initModel
    , tabs = TabsPlugin.initialModel
    }
