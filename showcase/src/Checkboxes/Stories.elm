module Checkboxes.Stories exposing (stories, update)

import Checkboxes.Model as Checkboxes
import Checkboxes.Msg as Checkboxes
import Element exposing (Element)
import Model exposing (Model)
import Msg exposing (Msg)
import PluginOptions exposing (defaultWithoutMenu)
import Return exposing (Return)
import UI.Checkbox as Checkbox
import UI.Internal.Basics exposing (ifThenElse)
import UI.RenderConfig exposing (RenderConfig)
import UI.Switch as Switch
import UI.Text as Text
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, goToDocsCallToAction, iconsSvgSprite, prettifyElmCode, storyWithModel)


update : Checkboxes.Msg -> Checkboxes.Model -> Return Checkboxes.Msg Checkboxes.Model
update msg model =
    case msg of
        Checkboxes.Box1Set newValue ->
            ( { model | box1 = newValue }, Cmd.none )

        Checkboxes.Box2Set newValue ->
            ( { model | box2 = newValue }, Cmd.none )

        Checkboxes.Box3Check ->
            ( { model | box3 = True }, Cmd.none )

        Checkboxes.Box3Uncheck ->
            ( { model | box3 = False }, Cmd.none )

        Checkboxes.SwitchSet newValue ->
            ( { model | switch = newValue }, Cmd.none )


stories : RenderConfig -> ExplorerUI
stories renderConfig =
    storiesOf
        "Checkboxes"
        [ boxDemo renderConfig ]


boxDemo : RenderConfig -> ExplorerStory
boxDemo renderConfig =
    storyWithModel
        ( "Checkboxes"
        , boxView renderConfig
        , { defaultWithoutMenu
            | code = boxCode
            , note = goToDocsCallToAction "Checkbox"
          }
        )


implicitMessages : Bool -> Checkboxes.Msg
implicitMessages newState =
    if newState then
        Checkboxes.Box3Check

    else
        Checkboxes.Box3Uncheck


boxView : RenderConfig -> Model -> Element Msg
boxView renderConfig ({ checkboxesStories } as model) =
    Element.column [ Element.spacing 8 ]
        [ iconsSvgSprite
        , Checkbox.checkbox "Extra ketchup (Free)"
            (Checkboxes.Box1Set >> Msg.CheckboxesStoriesMsg)
            checkboxesStories.box1
            |> Checkbox.renderElement renderConfig
        , Checkbox.checkbox "Large french fries (+0.50 USD)"
            (Checkboxes.Box2Set >> Msg.CheckboxesStoriesMsg)
            checkboxesStories.box2
            |> Checkbox.renderElement renderConfig
        , Checkbox.checkbox "Heinz® Mayonnaise (+0.75 USD)"
            (implicitMessages >> Msg.CheckboxesStoriesMsg)
            checkboxesStories.box3
            |> Checkbox.renderElement renderConfig
        , switchView renderConfig model
        , ("Total: "
            ++ (String.fromFloat <| totalPrice checkboxesStories)
            ++ " USD"
          )
            |> Text.body1
            |> Text.renderElement renderConfig
        ]


switchView : RenderConfig -> Model -> Element Msg
switchView renderConfig { checkboxesStories } =
    let
        label =
            "Donate 10¢ to charity"
    in
    Element.row [ Element.spacing 8 ]
        [ Switch.switch label
            (Checkboxes.SwitchSet >> Msg.CheckboxesStoriesMsg)
            checkboxesStories.switch
            |> Switch.withActivatedColor Switch.success
            |> Switch.renderElement renderConfig
        , Element.text label
        ]


totalPrice : Checkboxes.Model -> Float
totalPrice { box2, box3 } =
    List.sum
        [ 4.8
        , ifThenElse box2 0.5 0
        , ifThenElse box3 0.75 0
        ]


boxCode : String
boxCode =
    prettifyElmCode """
    Element.column [ Element.spacing 8 ]
        [ Checkbox.checkbox renderConfig
            Msg.Box1Set
            "Extra ketchup (Free)"
            model.box1
            |> Checkbox.renderElement renderConfig
        , Checkbox.checkbox renderConfig
            Msg.Box2Set
            "Large french fries (+0.50 USD)"
            model.box2
            |> Checkbox.renderElement renderConfig
        , Checkbox.checkbox renderConfig
            (\\newState -> ifThenElse newState Msg.Box3Check Msg.Box3Uncheck)
            "Heinz® Mayonnaise (+0.75 USD)"
            model.box3
            |> Checkbox.renderElement renderConfig
        , Switch.switch "Donate 10¢ to charity"
            Msg.SwitchSet
            model.switch
            |> Switch.withActivatedColor Switch.success
            |> Switch.renderElement renderConfig
            |> appenSwitchLabel
        , ("Total: "
            ++ (String.fromFloat <| totalPrice model)
            ++ " USD"
          )
            |> Text.body1
            |> Text.renderElement renderConfig
        ]
"""
