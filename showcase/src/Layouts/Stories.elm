module Layouts.Stories exposing (stories, update)

import Element exposing (Element)
import Layouts.Model as LayoutsModel
import Layouts.Msg as LayoutsMsg
import Model exposing (Model)
import Msg exposing (Msg)
import PluginOptions exposing (defaultWithoutMenu)
import Return exposing (Return)
import UI.Badge as Badge exposing (Badge)
import UI.Button as Button
import UI.Layout.Auth as Auth
import UI.RenderConfig exposing (RenderConfig)
import UI.TextField as TextField
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, goToDocsCallToAction, prettifyElmCode, storyList, storyWithModel)


update : LayoutsMsg.Msg -> LayoutsModel.Model -> Return LayoutsMsg.Msg LayoutsModel.Model
update msg model =
    case msg of
        LayoutsMsg.SetEmail e ->
            ( { model | email = e }, Cmd.none )

        LayoutsMsg.SetPassword p ->
            ( { model | password = p }, Cmd.none )

        LayoutsMsg.NoOp ->
            ( model, Cmd.none )


stories : RenderConfig -> ExplorerUI
stories cfg =
    storiesOf
        "Layouts"
        [ demo cfg
        ]


demo : RenderConfig -> ExplorerStory
demo renderConfig =
    storyWithModel
        ( "Auth"
        , view renderConfig
        , { defaultWithoutMenu
            | code = code
            , note = goToDocsCallToAction "Radio"
          }
        )


view : RenderConfig -> Model -> Element Msg
view renderConfig { layoutsStories } =
    let
        msg =
            Msg.LayoutsStoriesMsg
    in
    Auth.view renderConfig
        { title = "Auth Layout Example Story"
        , logo = Element.image [] { src = "logo.png", description = "logo" }
        , emailField = TextField.username (msg << LayoutsMsg.SetEmail) "username" layoutsStories.email
        , passwordField = TextField.username (msg << LayoutsMsg.SetPassword) "password" layoutsStories.password
        , submitMsg = msg LayoutsMsg.NoOp
        , submitButton = Button.fromLabel "Login" |> Button.cmd (msg LayoutsMsg.NoOp) Button.primary
        }


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
