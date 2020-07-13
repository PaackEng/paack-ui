module UI.RadioButton exposing (radioGroup)

{-|


# Atomic Elements

@docs checkbox, radioButton

**NOTE**:
We have plans for "checklists" and "radioGroups", but this won't be created until usecase appears.

-}

import Element exposing (Element)
import UI.Button as Button
import UI.Internal.Size as Size
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text


radioButton : RenderConfig -> msg -> String -> Bool -> Element msg
radioButton renderConfig message label state =
    Element.row [ Element.spacing 8 ]
        [ Button.toggle "Select"
            (always message)
            state
            |> Button.withSize Size.extraSmall
            |> Button.renderElement renderConfig
        , Text.caption label
            |> Text.renderElement renderConfig
        ]


radioGroup : RenderConfig -> (a -> msg) -> Maybe a -> List ( a, String ) -> Element msg
radioGroup renderConfig message selected buttons =
    buttons
        |> List.map
            (\( id, label ) ->
                radioButton renderConfig
                    (message id)
                    label
                    (selected == Just id)
            )
        |> Element.column [ Element.spacing 8 ]
