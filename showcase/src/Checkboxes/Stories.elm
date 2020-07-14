module Checkboxes.Stories exposing (stories, update)

import Checkboxes.Model as Checkboxes
import Checkboxes.Msg as Checkboxes
import Element
import Msg
import Return as R exposing (Return)
import UI.Checkbox as Checkbox
import UI.Internal.Basics exposing (ifThenElse)
import UI.Text as Text
import UIExplorer exposing (storiesOf)
import Utils exposing (iconsSvgSprite, storyWithModel)


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


stories renderConfig =
    storiesOf
        "Checkboxes"
        [ boxDemo renderConfig ]


boxDemo renderConfig =
    let
        implicitMessages newState =
            if newState then
                Checkboxes.Box3Check

            else
                Checkboxes.Box3Uncheck

        view { checkboxesStories } =
            Element.column [ Element.spacing 8 ]
                [ iconsSvgSprite
                , Checkbox.checkbox renderConfig
                    (Checkboxes.Box1Set >> Msg.CheckboxesStoriesMsg)
                    "Extra ketchup (+0.50 USD)"
                    checkboxesStories.box1
                , Checkbox.checkbox renderConfig
                    (Checkboxes.Box2Set >> Msg.CheckboxesStoriesMsg)
                    "Large french fries (+0.50 USD)"
                    checkboxesStories.box2
                , Checkbox.checkbox renderConfig
                    (implicitMessages >> Msg.CheckboxesStoriesMsg)
                    "Heinz® Mayonnaise (+0.75 USD)"
                    checkboxesStories.box3
                , ("Total: "
                    ++ (String.fromFloat <| totalPrice checkboxesStories)
                    ++ " USD"
                  )
                    |> Text.body1
                    |> Text.renderElement renderConfig
                ]

        totalPrice { box2, box3 } =
            List.sum
                [ 4.8
                , ifThenElse box2 0.5 0
                , ifThenElse box3 0.75 0
                ]
    in
    storyWithModel ( "Checkboxes", view, { note = """
This demo can be reproduced with the following code:

```elm
    Element.column [ Element.spacing 8 ]
        [ Checkbox.checkbox renderConfig
            Msg.Box1Set
            "Extra ketchup (+0.50 USD)"
            model.box1
        , Checkbox.checkbox renderConfig
            Msg.Box2Set
            "Large french fries (+0.50 USD)"
            model.box2
        , Checkbox.checkbox renderConfig
            (\\newState -> ifThenElse newState Msg.Box3Check Msg.Box3Uncheck)
            "Heinz® Mayonnaise (+0.75 USD)"
            model.box3
        , ("Total: "
            ++ (String.fromFloat <| totalPrice model)
            ++ " USD"
          )
            |> Text.body1
            |> Text.renderElement renderConfig
        ]
```""" } )
