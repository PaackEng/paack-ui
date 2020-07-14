module Radio.Stories exposing (stories, update)

import Element
import Msg
import Radio.Model as RadioModel
import Radio.Msg as RadioMsg
import Return as R exposing (Return)
import UI.Internal.Basics exposing (ifThenElse)
import UI.Radio as Radio
import UI.Text as Text
import UIExplorer exposing (storiesOf)
import Utils exposing (iconsSvgSprite, storyWithModel)


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
                , Radio.group renderConfig
                    label
                    (RadioMsg.Set >> Msg.RadioStoriesMsg)
                    radioStories.selected
                    [ ( RadioModel.Queen, "Queen" )
                    , ( RadioModel.Beatles, "Beatles" )
                    , ( RadioModel.ACDC, "AC/DC" )
                    , ( RadioModel.LedZeppelin, "Led Zeppelin" )
                    , ( RadioModel.PinkFloyd, "Pink Floyd" )
                    ]
                ]
    in
    storyWithModel ( "Radio", view, { note = """
This demo can be reproduced with the following code:

```elm
    Radio.group renderConfig
        "Pick one classic rock band"
        Msg.RadioSet
        model.selected
        [ ( Queen, "Queen" )
        , ( Beatles, "Beatles" )
        , ( ACDC, "AC/DC" )
        , ( LedZeppelin, "Led Zeppelin" )
        , ( PinkFloyd, "Pink Floyd" )
        ]
```""" } )
