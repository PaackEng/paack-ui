module Layouts.Stories exposing (stories, update)

import Layouts.Model as LayoutsModel
import Layouts.Msg as LayoutsMsg
import Msg exposing (Msg)
import PluginOptions exposing (defaultWithMenu)
import Return exposing (Return)
import UI.Badge as Badge exposing (Badge)
import UI.Layout.Auth as Auth
import UI.RenderConfig exposing (RenderConfig)
import UI.TextField as TextField
import UI.Button as Button
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, goToDocsCallToAction, prettifyElmCode, storyList)
import Element


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
            ( "Auth"
            , [ Auth.view cfg
                    { title = "Auth Layout Example Story"
                    , logo = Element.image [] { src = "logo.png", description = "logo" }
                    , emailField = TextField.username (always msg) "username" ""
                    , passwordField = TextField.username (always msg) "username" ""
                    , submitMsg = msg
                    , submitButton = Button.fromLabel "Login" |> Button.cmd msg Button.primary
                    }
                ]
            , { defaultWithMenu | code = code }
            )
        ]


code : String
code =
    prettifyElmCode """
Auth.view cfg
    { title = "Auth Layout Example Story"
    , logo = Element.image [] { src = "logo.png", description = "logo" }
    , emailField = TextField.username (always msg) "username" ""
    , passwordField = TextField.username (always msg) "username" ""
    , submitMsg = msg
    , submitButton = Button.fromLabel "Login" |> Button.cmd msg Button.primary
    }
"""
