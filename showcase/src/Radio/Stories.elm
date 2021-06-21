module Radio.Stories exposing (stories, update)

import Element exposing (Element)
import Model exposing (Model)
import Msg exposing (Msg)
import PluginOptions exposing (defaultWithMenu)
import Radio.Model as RadioModel exposing (Options)
import Radio.Msg as RadioMsg
import Return exposing (Return)
import UI.Effect as Effect
import UI.Radio as Radio
import UI.RenderConfig exposing (RenderConfig)
import UIExplorer exposing (storiesOf)
import Utils
    exposing
        ( ExplorerStory
        , ExplorerUI
        , goToDocsCallToAction
        , iconsSvgSprite
        , prettifyElmCode
        , story
        , storyWithModel
        )


update : RadioMsg.Msg -> RadioModel.Model -> Return Msg.Msg RadioModel.Model
update msg model =
    case msg of
        RadioMsg.Set id newValue ->
            ( { model | selected = Just newValue }
            , Effect.domFocus (RadioMsg.FocusResult >> Msg.RadioStoriesMsg) id
                |> Effect.perform
            )

        RadioMsg.FocusResult _ ->
            ( model, Cmd.none )

        RadioMsg.NoOp ->
            ( model, Cmd.none )


stories : RenderConfig -> ExplorerUI
stories renderConfig =
    storiesOf
        "Radio"
        [ radioGroupVertical renderConfig
        , radioGroupHorizontal renderConfig
        , radioGroupMedium renderConfig
        , united renderConfig
        ]


radioGroupVertical : RenderConfig -> ExplorerStory
radioGroupVertical renderConfig =
    storyWithModel
        ( "Vertical"
        , view Radio.vertical Radio.sizeSM renderConfig
        , { defaultWithMenu
            | code = codeForVerticalRadioGroup
            , note = goToDocsCallToAction "Radio"
          }
        )


radioGroupHorizontal : RenderConfig -> ExplorerStory
radioGroupHorizontal renderConfig =
    storyWithModel
        ( "Horizontal"
        , view Radio.horizontal Radio.sizeSM renderConfig
        , { defaultWithMenu
            | code = codeForHorizontalRadioGroup
            , note = goToDocsCallToAction "Radio"
          }
        )


radioGroupMedium : RenderConfig -> ExplorerStory
radioGroupMedium renderConfig =
    storyWithModel
        ( "Medium"
        , view Radio.vertical Radio.sizeMD renderConfig
        , { defaultWithMenu
            | code = codeForHorizontalRadioGroup
            , note = goToDocsCallToAction "Radio"
          }
        )


united : RenderConfig -> ExplorerStory
united renderConfig =
    story
        ( "United"
        , unitedView renderConfig
        , defaultWithMenu
        )


label : String
label =
    "Pick one classic rock band"


view : Radio.Direction -> Radio.RadioSize -> RenderConfig -> Model -> Element Msg
view direction size renderConfig { radioStories } =
    radioGroupView direction size renderConfig (\id value -> Msg.RadioStoriesMsg <| RadioMsg.Set id value) radioStories


radioGroupView : Radio.Direction -> Radio.RadioSize -> RenderConfig -> (String -> Options -> msg) -> RadioModel.Model -> Element msg
radioGroupView direction size renderConfig msg { selected } =
    Element.column
        [ Element.spacing 8 ]
        [ iconsSvgSprite
        , Radio.group
            { label = label
            , onSelectMsg = msg
            , idPrefix = "band-radio"
            }
            |> Radio.withSelected selected
            |> Radio.withDirection direction
            |> Radio.withSize size
            |> Radio.withButtons
                [ Radio.button RadioModel.Queen "Queen"
                , Radio.button RadioModel.Beatles "Beatles"
                , Radio.button RadioModel.ACDC "AC/DC"
                , Radio.button RadioModel.LedZeppelin "Led Zeppelin"
                , Radio.button RadioModel.PinkFloyd "Pink Floyd"
                ]
            |> Radio.renderElement renderConfig
        ]


unitedView : RenderConfig -> Element Msg
unitedView renderConfig =
    Element.column
        [ Element.spacing 8 ]
        [ radioGroupView Radio.vertical Radio.sizeSM renderConfig (\_ _ -> Msg.RadioStoriesMsg RadioMsg.NoOp) { selected = Nothing }
        , radioGroupView Radio.horizontal Radio.sizeSM renderConfig (\_ _ -> Msg.RadioStoriesMsg RadioMsg.NoOp) { selected = Nothing }
        , radioGroupView Radio.vertical Radio.sizeMD renderConfig (\_ _ -> Msg.RadioStoriesMsg RadioMsg.NoOp) { selected = Nothing }
        ]


codeForVerticalRadioGroup : String
codeForVerticalRadioGroup =
    prettifyElmCode """
Radio.group
    { label = "Pick one classic rock band"
    , onSelectMsg = Msg.RadioSet
    , idPrefix = "band-radio"
    }
    |> Radio.withSelected model.selected
    |> Radio.withButtons
        [ Radio.button Model.Queen "Queen"
        , Radio.button Model.Beatles "Beatles"
        , Radio.button Model.ACDC "AC/DC"
        , Radio.button Model.LedZeppelin "Led Zeppelin"
        , Radio.button Model.PinkFloyd "Pink Floyd"
        ]
    |> Radio.renderElement renderConfig
"""


codeForHorizontalRadioGroup : String
codeForHorizontalRadioGroup =
    prettifyElmCode """
Radio.group
    { label = "Pick one classic rock band"
    , onSelectMsg = Msg.RadioSet
    , idPrefix = "band-radio"
    }
    |> Radio.withSelected model.selected
    |> Radio.withDirection model.direction
    |> Radio.withButtons
        [ Radio.button Model.Queen "Queen"
        , Radio.button Model.Beatles "Beatles"
        , Radio.button Model.ACDC "AC/DC"
        , Radio.button Model.LedZeppelin "Led Zeppelin"
        , Radio.button Model.PinkFloyd "Pink Floyd"
        ]
    |> Radio.renderElement renderConfig
"""
