module Main exposing (CompileDocument, main)

import Alerts
import Badges
import Buttons.Stories as Buttons
import Checkboxes.Stories as Checkboxes
import ContentPlaceholders
import Datepicker.Stories as Datepicker
import Dialog
import Dropdown.Stories as Dropdowns
import Filters.Stories as Filters
import Html exposing (Html, div, img)
import Html.Attributes exposing (src, style)
import Icons
import LoadingView
import Menu.Stories as Menu
import Model exposing (Model)
import Msg exposing (Msg(..))
import Paginators.Stories as Paginators
import Palette
import PluginOptions exposing (PluginOptions)
import Radio.Stories as Radio
import Return as R
import Sidebar.Stories as Sidebar
import Sizes
import Switches.Stories as Switches
import Tables.Stories as Tables
import Tabs.Stories as Tabs
import TextField
import Texts
import Tile.Stories as Tile
import UI.Document exposing (Document)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UIExplorer
    exposing
        ( Config
        , UIExplorerProgram
        , category
        , createCategories
        , exploreWithCategories
        , logoFromHtml
        )
import UIExplorer.Plugins.Code as CodePlugin
import UIExplorer.Plugins.MenuVisibility as MenuVisibility
import UIExplorer.Plugins.Note as NotePlugin
import UIExplorer.Plugins.Tabs as TabsPlugin
import UIExplorer.Plugins.Tabs.Icons as TabsIconsPlugin
import Utils exposing (ExplorerModel)


{-| importing UI.Document mostly so they compile too
-}
type alias CompileDocument =
    Document Never Never


config : Config Model Msg PluginOptions
config =
    { customModel = Model.init
    , customHeader = customHeader
    , update = updateStories
    , onModeChanged = Nothing
    , viewEnhancer = viewEnhancer
    , menuViewEnhancer = MenuVisibility.menuViewEnhancer
    , subscriptions = always Sub.none
    }


customHeader : Maybe (UIExplorer.CustomHeader Msg)
customHeader =
    Just
        { title = ""
        , logo = logoFromHtml logo
        , titleColor = Just "#000000"
        , bgColor = Just "#f6f6f6"
        }


viewEnhancer : ExplorerModel -> Html (UIExplorer.Msg Msg) -> Html (UIExplorer.Msg Msg)
viewEnhancer m stories =
    Html.div []
        [ stories
        , TabsPlugin.view m.colorMode
            m.customModel.tabs
            [ ( "Code", CodePlugin.viewEnhancer m, TabsIconsPlugin.code )
            , ( "Notes", NotePlugin.viewEnhancer m, TabsIconsPlugin.note )
            ]
            TabMsg
        ]


renderConfig : RenderConfig
renderConfig =
    RenderConfig.init
        { width = 1920
        , height = 1080
        }
        RenderConfig.localeEnglish


main : UIExplorerProgram Model Msg PluginOptions
main =
    createCategories
        |> category
            "Styles"
            [ Palette.stories
            , Sizes.stories renderConfig
            ]
        |> category
            "Basics"
            [ Texts.stories renderConfig
            , Icons.stories renderConfig
            , Buttons.stories renderConfig
            , Alerts.stories renderConfig
            , Badges.stories renderConfig
            , Dialog.stories renderConfig
            , TextField.stories renderConfig
            , LoadingView.stories
            , Menu.stories renderConfig
            , Checkboxes.stories renderConfig
            , Switches.stories renderConfig
            , Radio.stories renderConfig
            , Tabs.stories renderConfig
            , Tile.stories renderConfig
            , ContentPlaceholders.stories renderConfig
            , Dropdowns.stories renderConfig
            , Datepicker.stories renderConfig
            ]
        |> category
            "Complex components"
            [ Tables.stories renderConfig
            , Paginators.stories renderConfig
            , Sidebar.stories renderConfig
            , Filters.stories renderConfig
            ]
        |> exploreWithCategories config


logo : Html msg
logo =
    div
        [ style "padding" "0 20px"
        , style "display" "flex"
        , style "align-items" "center"
        ]
        [ img [ src "logo.svg", style "width" "112px" ] [] ]


updateStories : Msg -> ExplorerModel -> ( ExplorerModel, Cmd Msg )
updateStories msg ({ customModel } as model) =
    case msg of
        ButtonsStoriesMsg subMsg ->
            Buttons.update subMsg customModel.buttonsStories
                |> R.map (\t -> { customModel | buttonsStories = t })
                |> finishCustomUpdate model

        PaginatorsStoriesMsg subMsg ->
            Paginators.update subMsg customModel.paginatorsStories
                |> R.map (\t -> { customModel | paginatorsStories = t })
                |> finishCustomUpdate model

        TablesStoriesMsg subMsg ->
            Tables.update subMsg customModel.tablesStories
                |> R.map (\t -> { customModel | tablesStories = t })
                |> finishCustomUpdate model

        CheckboxesStoriesMsg subMsg ->
            Checkboxes.update subMsg customModel.checkboxesStories
                |> R.map (\t -> { customModel | checkboxesStories = t })
                |> finishCustomUpdate model

        SwitchesStoriesMsg subMsg ->
            Switches.update subMsg customModel.switchesStories
                |> R.map (\t -> { customModel | switchesStories = t })
                |> finishCustomUpdate model

        RadioStoriesMsg subMsg ->
            Radio.update subMsg customModel.radioStories
                |> R.map (\t -> { model | customModel = { customModel | radioStories = t } })

        TabsStoriesMsg subMsg ->
            Tabs.update subMsg customModel.tabsStories
                |> R.map (\t -> { customModel | tabsStories = t })
                |> finishCustomUpdate model

        TileStoriesMsg subMsg ->
            Tile.update subMsg customModel.tileStories
                |> R.map (\t -> { customModel | tileStories = t })
                |> finishCustomUpdate model

        TabMsg submsg ->
            TabsPlugin.update submsg customModel.tabs
                |> (\t -> ( { customModel | tabs = t }, Cmd.none ))
                |> finishCustomUpdate model

        SidebarStoriesMsg submsg ->
            Sidebar.update submsg customModel.sidebarStories
                |> R.map (\t -> { customModel | sidebarStories = t })
                |> finishCustomUpdate model

        DropdownStoriesMsg submsg ->
            Dropdowns.update renderConfig submsg customModel.dropdownStories
                |> R.map (\t -> { model | customModel = { customModel | dropdownStories = t } })

        FiltersStoriesMsg subMsg ->
            Filters.update subMsg customModel.filtersStories
                |> R.map (\t -> { model | customModel = { customModel | filtersStories = t } })

        MenuStoriesMsg subMsg ->
            Menu.update subMsg customModel.menuStories
                |> R.map (\t -> { model | customModel = { customModel | menuStories = t } })

        NoOp ->
            ( model, Cmd.none )

        DatepickerStoriesMsg submsg ->
            Datepicker.update renderConfig submsg customModel.datepickerStories
                |> R.map (\t -> { model | customModel = { customModel | datepickerStories = t } })


finishCustomUpdate : ExplorerModel -> ( Model, Cmd msg ) -> ( ExplorerModel, Cmd Msg )
finishCustomUpdate model ( newCustomModel, _ ) =
    ( { model | customModel = newCustomModel }
    , Cmd.none
    )
