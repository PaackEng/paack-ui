module Main exposing (main)

import Alerts
import Badges
import Buttons.Stories as Buttons
import Element exposing (Element)
import Element.Font as Font
import Html exposing (Html, i)
import Html.Attributes exposing (class, style)
import Icons
import LoadingView as LoadingView
import Model as Model exposing (Model)
import Msg exposing (Msg(..))
import Paginators.Stories as Paginators
import Palette
import Return as R
import Sizes
import Tables.Stories as Tables
import TextField
import Texts
import UI.NavigationContainer
import UI.RenderConfig exposing (RenderConfig)
import UI.RowList
import UIExplorer exposing (Config, UIExplorerProgram, explore, logoFromHtml)
import UIExplorer.Plugins.Note as Note
import Utils exposing (story)



{- importing UI.NavigationContainer and UI.RowList mostly so they compile too -}


type alias PluginOptions =
    { note : String }


config : Config Model Msg PluginOptions
config =
    { customModel = Model.init
    , customHeader =
        Just
            { title = ""
            , logo = logoFromHtml logo
            , titleColor = Just "#FFF"
            , bgColor = Just "#1247D0"
            }
    , update = updateStories
    , viewEnhancer = Note.viewEnhancer
    , menuViewEnhancer = \_ v -> v
    , subscriptions = always Sub.none
    }


main : UIExplorerProgram Model Msg PluginOptions
main =
    let
        renderConfig =
            UI.RenderConfig.fromWindow
                { width = 1920
                , height = 1080
                }
    in
    explore
        config
        [ Texts.stories renderConfig
        , Icons.stories renderConfig
        , Palette.stories
        , Buttons.stories renderConfig
        , Alerts.stories renderConfig
        , Badges.stories renderConfig
        , TextField.stories renderConfig
        , LoadingView.stories
        , Tables.stories renderConfig
        , Paginators.stories renderConfig
        , Sizes.stories renderConfig
        ]


logo : Html msg
logo =
    i
        [ style "color" "white"
        , style "height" "100%"
        , style "font-size" "30px"
        , style "display" "flex"
        , style "align-items" "center"
        , style "padding" "0 20px"
        ]
        []


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

        NoOp ->
            ( model, Cmd.none )
