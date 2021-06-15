module Msg exposing (Msg(..))

import Buttons.Msg as Buttons
import Checkboxes.Msg as Checkboxes
import Dropdown.Msg as Dropdown
import Layouts.Msg as Layouts
import Paginators.Msg as Paginators
import Radio.Msg as Radio
import Sidebar.Msg as Sidebar
import Switches.Msg as Switches
import Tables.Msg as Tables
import Tabs.Msg as Tabs
import UIExplorer.Plugins.Tabs as TabsPlugin


type Msg
    = ButtonsStoriesMsg Buttons.Msg
    | PaginatorsStoriesMsg Paginators.Msg
    | TablesStoriesMsg Tables.Msg
    | CheckboxesStoriesMsg Checkboxes.Msg
    | SwitchesStoriesMsg Switches.Msg
    | RadioStoriesMsg Radio.Msg
    | LayoutsStoriesMsg Layouts.Msg
    | SidebarStoriesMsg Sidebar.Msg
    | TabsStoriesMsg Tabs.Msg
    | TabMsg TabsPlugin.Msg
    | DropdownStoriesMsg Dropdown.Msg
    | NoOp
