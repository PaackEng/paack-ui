module Radio.Stories exposing (stories, update)

import Element
import Msg
import PluginOptions exposing (defaultWithoutMenu)
import Radio.Model as RadioModel
import Radio.Msg as RadioMsg
import Return as R exposing (Return)
import UI.Internal.Basics exposing (ifThenElse)
import UI.Radio as Radio
import UI.Text as Text
import UIExplorer exposing (storiesOf)
import Utils exposing (goToDocsCallToAction, iconsSvgSprite, prettifyElmCode, storyWithModel)


update : RadioMsg.Msg -> RadioModel.Model -> Return RadioMsg.Msg RadioModel.Model
update msg model =
    case msg of
        RadioMsg.Set newValue ->
            ( { model | selected = Just newValue }, Cmd.none )


stories renderConfig =
    storiesOf
        "Radio"
        [ demo renderConfig ]


demo renderConfig =
    let
        label =
            "Pick one classic rock band"

        view { radioStories } =
            Element.column
                [ Element.spacing 8 ]
                [ iconsSvgSprite
                , Text.body2 label
                    |> Text.renderElement renderConfig
                , Radio.group
                    label
                    (RadioMsg.Set >> Msg.RadioStoriesMsg)
                    |> Radio.withSelected radioStories.selected
                    |> Radio.withButtons
                        [ Radio.button RadioModel.Queen "Queen"
                        , Radio.button RadioModel.Beatles "Beatles"
                        , Radio.button RadioModel.ACDC "AC/DC"
                        , Radio.button RadioModel.LedZeppelin "Led Zeppelin"
                        , Radio.button RadioModel.PinkFloyd "Pink Floyd"
                        ]
                    |> Radio.renderElement renderConfig
                ]
    in
    storyWithModel
        ( "Radio"
        , view
        , { defaultWithoutMenu
            | code = prettifyElmCode """
Radio.group
    "Pick one classic rock band"
    Msg.RadioSet
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
            , note = goToDocsCallToAction "Radio"
          }
        )
