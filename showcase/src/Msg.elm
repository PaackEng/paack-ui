module Msg exposing (Msg(..))

import Buttons.Msg as Buttons
import Checkboxes.Msg as Checkboxes
import Layouts.Msg as Layouts
import Paginators.Msg as Paginators
import Radio.Msg as Radio
import Sidebar.Msg as Sidebar
import Tables.Msg as Tables
import Tabs.Msg as Tabs
import UIExplorer.Plugins.Tabs as TabsPlugin


type Msg
    = ButtonsStoriesMsg Buttons.Msg
    | PaginatorsStoriesMsg Paginators.Msg
    | TablesStoriesMsg Tables.Msg
    | CheckboxesStoriesMsg Checkboxes.Msg
    | RadioStoriesMsg Radio.Msg
    | LayoutsStoriesMsg Layouts.Msg
    | SidebarStoriesMsg Sidebar.Msg
    | TabsStoriesMsg Tabs.Msg
    | TabMsg TabsPlugin.Msg
    | NoOp
