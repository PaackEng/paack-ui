module Model exposing
    ( Model
    , init
    )

import Buttons.Model as Buttons
import Checkboxes.Model as Checkboxes
import Paginators.Model as Paginators
import Navigators.Model as Navigators
import Radio.Model as Radio
import Tables.Model as Tables
import Tabs.Model as Tabs
import UI.RenderConfig as RenderConfig
import UIExplorer.Plugins.Tabs as TabsPlugin


type alias Model =
    { buttonsStories : Buttons.Model
    , paginatorsStories : Paginators.Model
    , tablesStories : Tables.Model
    , checkboxesStories : Checkboxes.Model
    , radioStories : Radio.Model
    , tabsStories : Tabs.Model
    , navigatorsStories : Navigators.Model 
    , tabs : TabsPlugin.Model
    }


init : Model
init =
    let
        renderConfig =
            RenderConfig.init
                { width = 1920
                , height = 1080
                }
                RenderConfig.localeEnglish
    in
    { buttonsStories = Buttons.initModel
    , paginatorsStories = Paginators.initModel
    , tablesStories = Tables.initModel
    , checkboxesStories = Checkboxes.initModel
    , radioStories = Radio.initModel
    , tabsStories = Tabs.initModel
    , navigatorsStories = Navigators.initModel renderConfig 
    , tabs = TabsPlugin.initialModel
    }
