module Model exposing
    ( Model
    , init
    )

import Buttons.Model as Buttons
import Checkboxes.Model as Checkboxes
import Dropdown.Model as Dropdown
import Layouts.Model as Layouts
import Paginators.Model as Paginators
import Radio.Model as Radio
import Sidebar.Model as Sidebar
import Switches.Model as Switches
import Tables.Model as Tables
import Tabs.Model as Tabs
import UIExplorer.Plugins.Tabs as TabsPlugin


type alias Model =
    { buttonsStories : Buttons.Model
    , paginatorsStories : Paginators.Model
    , tablesStories : Tables.Model
    , checkboxesStories : Checkboxes.Model
    , switchesStories : Switches.Model
    , radioStories : Radio.Model
    , tabsStories : Tabs.Model
    , tabs : TabsPlugin.Model
    , layoutsStories : Layouts.Model
    , sidebarStories : Sidebar.Model
    , dropdownStories : Dropdown.Model
    }


init : Model
init =
    { buttonsStories = Buttons.initModel
    , paginatorsStories = Paginators.initModel
    , tablesStories = Tables.initModel
    , checkboxesStories = Checkboxes.initModel
    , switchesStories = Switches.initModel
    , radioStories = Radio.initModel
    , tabsStories = Tabs.initModel
    , tabs = TabsPlugin.initialModel
    , layoutsStories = Layouts.initModel
    , sidebarStories = Sidebar.initModel
    , dropdownStories = Dropdown.initModel
    }
