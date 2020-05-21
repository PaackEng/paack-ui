module Main exposing (main)

import ActionBars
import Alerts
import Badges
import Buttons.Stories as Buttons
import Element exposing (Element)
import Element.Font as Font
import Form.Checkboxes as Checkboxes
import Form.State as FormState
import Form.TextField as TextField
import Html exposing (Html, i)
import Html.Attributes exposing (class, style)
import Icons
import LoadingView as LoadingView
import Model as Model exposing (Model)
import Msg exposing (Msg(..))
import Palette
import PopUps as PopUps
import Return as R
import Tables
import Texts
import UI.RenderConfig exposing (RenderConfig)
import UIExplorer exposing (Config, UIExplorerProgram, explore, logoFromHtml)
import UIExplorer.Plugins.Note as Note
import Utils exposing (story)


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
        , ActionBars.stories renderConfig
        , Alerts.stories renderConfig
        , PopUps.stories
        , Badges.stories renderConfig
        , TextField.stories renderConfig
        , Checkboxes.stories
        , LoadingView.stories
        , Tables.stories renderConfig
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
        FormStoriesMsg subMsg ->
            let
                newFormStories =
                    FormState.update subMsg customModel.formStories
            in
            ( { model
                | customModel = { customModel | formStories = newFormStories }
              }
            , Cmd.none
            )

        ButtonsStoriesMsg subMsg ->
            Buttons.update subMsg customModel.buttonsStories
                |> R.map (\t -> { customModel | buttonsStories = t })
                |> R.map (\newCustomModel -> { model | customModel = newCustomModel })
                |> Tuple.mapSecond (always Cmd.none)

        NoOp ->
            ( model, Cmd.none )
