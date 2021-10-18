module Model exposing
    ( Model
    , init
    )

import Buttons.Model as Buttons
import Checkboxes.Model as Checkboxes
import Dropdown.Model as Dropdown
import Filters.Model as Filters
import Paginators.Model as Paginators
import Radio.Model as Radio
import Sidebar.Model as Sidebar
import Switches.Model as Switches
import Tables.Model as Tables
import Tabs.Model as Tabs
import Tile.Model as Tile
import UIExplorer.Plugins.Tabs as TabsPlugin


type alias Model =
    { buttonsStories : Buttons.Model
    , checkboxesStories : Checkboxes.Model
    , dropdownStories : Dropdown.Model
    , filtersStories : Filters.Model
    , paginatorsStories : Paginators.Model
    , radioStories : Radio.Model
    , sidebarStories : Sidebar.Model
    , switchesStories : Switches.Model
    , tablesStories : Tables.Model
    , tabsStories : Tabs.Model
    , tileStories : Tile.Model
    , tabs : TabsPlugin.Model
    }


init : Model
init =
    { buttonsStories = Buttons.initModel
    , checkboxesStories = Checkboxes.initModel
    , dropdownStories = Dropdown.initModel
    , filtersStories = Filters.initModel
    , paginatorsStories = Paginators.initModel
    , radioStories = Radio.initModel
    , sidebarStories = Sidebar.initModel
    , switchesStories = Switches.initModel
    , tablesStories = Tables.initModel
    , tabsStories = Tabs.initModel
    , tileStories = Tile.initModel
    , tabs = TabsPlugin.initialModel
    }
