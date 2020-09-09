module Layouts.Stories exposing (stories, update)

import Layouts.Model as LayoutsModel
import Layouts.Msg as LayoutsMsg
import Msg exposing (Msg)
import PluginOptions exposing (defaultWithMenu)
import Return exposing (Return)
import UI.Badge as Badge exposing (Badge)
import UI.Layout.Login as Login
import UI.RenderConfig exposing (RenderConfig)
import UI.TextField as TextField
import UI.Button as Button
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, goToDocsCallToAction, prettifyElmCode, storyList)


update : LayoutsMsg.Msg -> LayoutsModel.Model -> Return LayoutsMsg.Msg LayoutsModel.Model
update msg model =
    ( {}, Cmd.none )


stories : RenderConfig -> ExplorerUI
stories cfg =
    let
        msg =
            Msg.LayoutsStoriesMsg LayoutsMsg.NoOp
    in
    storiesOf
        "Layouts"
        [ storyList
            ( "Login"
            , [ Login.view cfg
                    { title = "Login Layout Example Story"
                    , logoSrc = "logo.png"
                    , emailField = TextField.username (always msg) "username" ""
                    , passwordField = TextField.username (always msg) "username" ""
                    , submitMsg = msg
                    , submitButton = Button.fromLabel "Login" |> Button.cmd msg Button.primary
                    }
                ]
            , defaultWithMenu
            )
        ]
