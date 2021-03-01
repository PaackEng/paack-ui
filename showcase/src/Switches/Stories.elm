module Switches.Stories exposing (stories, update)

import Element exposing (Element)
import Model exposing (Model)
import Msg exposing (Msg)
import PluginOptions exposing (defaultWithoutMenu)
import Return exposing (Return)
import Switches.Model as Switches
import Switches.Msg as Switches
import UI.RenderConfig exposing (RenderConfig)
import UI.Switch as Switch exposing (Switch)
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, goToDocsCallToAction, iconsSvgSprite, prettifyElmCode, storyWithModel)


update : Switches.Msg -> Switches.Model -> Return Switches.Msg Switches.Model
update msg model =
    case msg of
        Switches.Switch1Set newValue ->
            ( { model | switch1 = newValue }, Cmd.none )

        Switches.Switch2Set newValue ->
            ( { model | switch2 = newValue }, Cmd.none )

        Switches.Switch3Check ->
            ( { model | switch3 = True }, Cmd.none )

        Switches.Switch3Uncheck ->
            ( { model | switch3 = False }, Cmd.none )


stories : RenderConfig -> ExplorerUI
stories renderConfig =
    storiesOf
        "Switches"
        [ switchDemo renderConfig ]


switchDemo : RenderConfig -> ExplorerStory
switchDemo renderConfig =
    storyWithModel
        ( "Switches"
        , switchesView renderConfig
        , { defaultWithoutMenu
            | code = switchCode
            , note = goToDocsCallToAction "Switch"
          }
        )


implicitMessages : Bool -> Switches.Msg
implicitMessages newState =
    if newState then
        Switches.Switch3Check

    else
        Switches.Switch3Uncheck


switchesView : RenderConfig -> Model -> Element Msg
switchesView renderConfig { switchesStories } =
    Element.column [ Element.spacing 8 ]
        [ iconsSvgSprite
        , switchView renderConfig "Friendly mode" <|
            \label ->
                Switch.default label
                    (Switches.Switch1Set >> Msg.SwitchesStoriesMsg)
                    switchesStories.switch1
        , switchView renderConfig "Firewall status" <|
            \label ->
                Switch.success label
                    (Switches.Switch2Set >> Msg.SwitchesStoriesMsg)
                    switchesStories.switch2
        , switchView renderConfig "Root access" <|
            \label ->
                Switch.danger label
                    (implicitMessages >> Msg.SwitchesStoriesMsg)
                    switchesStories.switch3
        ]


switchView : RenderConfig -> String -> (String -> Switch Msg) -> Element Msg
switchView renderConfig label switch =
    Element.row [ Element.spacing 8 ]
        [ Switch.renderElement renderConfig <| switch ("Toggle " ++ label)
        , Element.text label
        ]


switchCode : String
switchCode =
    prettifyElmCode """
    Element.column [ Element.spacing 8 ]
        [ Switch.default "Toggle friendly mode"
            Msg.Switch1Set
            model.switch1
            |> Switch.renderElement renderConfig
            |> appendSwitchLabel "Friendly mode"
        , Switch.success "Toggle firewall status"
            Msg.Switch2Set
            model.switch2
            |> Switch.renderElement renderConfig
            |> appendSwitchLabel  "Firewall status"
        , Switch.danger "Toggle root access"
            Msg.Switch2Set
            model.switch2
            |> Switch.renderElement renderConfig
            |> appendSwitchLabel  "Root access"
        ]
"""
