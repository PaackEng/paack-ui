module Main exposing (main)

import Alerts
import Badges
import Buttons.Stories as Buttons
import Checkboxes.Stories as Checkboxes
import Element exposing (Element)
import Element.Font as Font
import Html exposing (Html, div, img)
import Html.Attributes exposing (src, style)
import Icons
import LoadingView as LoadingView
import Model as Model exposing (Model)
import Msg exposing (Msg(..))
import Paginators.Stories as Paginators
import Palette
import PluginOptions exposing (PluginOptions)
import Radio.Stories as Radio
import Return as R
import Sizes
import Tables.Stories as Tables
import TextField
import Texts
import UI.ListView
import UI.NavigationContainer
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
import Utils exposing (story)



{- importing UI.NavigationContainer and UI.ListView mostly so they compile too -}


config : Config Model Msg PluginOptions
config =
    { customModel = Model.init
    , customHeader =
        Just
            { title = ""
            , logo = logoFromHtml logo
            , titleColor = Just "#000000"
            , bgColor = Just "#f6f6f6"
            }
    , update = updateStories
    , onModeChanged = Nothing
    , viewEnhancer =
        \m stories ->
            Html.div []
                [ stories
                , TabsPlugin.view m.colorMode
                    m.customModel.tabs
                    [ ( "Code", CodePlugin.viewEnhancer m, TabsIconsPlugin.code )
                    , ( "Notes", NotePlugin.viewEnhancer m, TabsIconsPlugin.note )
                    ]
                    TabMsg
                ]
    , menuViewEnhancer = MenuVisibility.menuViewEnhancer
    , subscriptions = always Sub.none
    }


main : UIExplorerProgram Model Msg PluginOptions
main =
    let
        renderConfig =
            RenderConfig.init
                { width = 1920
                , height = 1080
                }
                RenderConfig.localeEnglishGB
    in
    exploreWithCategories
        config
        (createCategories
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
                , TextField.stories renderConfig
                , LoadingView.stories
                , Checkboxes.stories renderConfig
                , Radio.stories renderConfig
                ]
            |> category
                "Complex components"
                [ Tables.stories renderConfig
                , Paginators.stories renderConfig
                ]
        )


logo : Html msg
logo =
    div
        [ style "padding" "0 20px"
        , style "display" "flex"
        , style "align-items" "center"
        ]
        [ img [ src "logo.png" ] [] ]


updateStories : Msg -> UIExplorer.Model Model Msg PluginOptions -> ( UIExplorer.Model Model Msg PluginOptions, Cmd Msg )
updateStories msg ({ customModel } as model) =
    case msg of
        ButtonsStoriesMsg subMsg ->
            Buttons.update subMsg customModel.buttonsStories
                |> R.map (\t -> { customModel | buttonsStories = t })
                |> R.map (\newCustomModel -> { model | customModel = newCustomModel })
                |> Tuple.mapSecond (always Cmd.none)

        PaginatorsStoriesMsg subMsg ->
            Paginators.update subMsg customModel.paginatorsStories
                |> R.map (\t -> { customModel | paginatorsStories = t })
                |> R.map (\newCustomModel -> { model | customModel = newCustomModel })
                |> Tuple.mapSecond (always Cmd.none)

        TablesStoriesMsg subMsg ->
            Tables.update subMsg customModel.tablesStories
                |> R.map (\t -> { customModel | tablesStories = t })
                |> R.map (\newCustomModel -> { model | customModel = newCustomModel })
                |> Tuple.mapSecond (always Cmd.none)

        CheckboxesStoriesMsg subMsg ->
            Checkboxes.update subMsg customModel.checkboxesStories
                |> R.map (\t -> { customModel | checkboxesStories = t })
                |> R.map (\newCustomModel -> { model | customModel = newCustomModel })
                |> Tuple.mapSecond (always Cmd.none)

        RadioStoriesMsg subMsg ->
            Radio.update subMsg customModel.radioStories
                |> R.map (\t -> { customModel | radioStories = t })
                |> R.map (\newCustomModel -> { model | customModel = newCustomModel })
                |> Tuple.mapSecond (always Cmd.none)

        TabMsg submsg ->
            ( { model | customModel = { customModel | tabs = TabsPlugin.update submsg customModel.tabs } }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )
