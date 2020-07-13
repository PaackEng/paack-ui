module UI.Checkbox exposing (checkbox)

{-|


# Atomic Elements

@docs checkbox

**NOTE**:
We have plans for "checklists", but this won't be created until usecase appears.

-}

import Element exposing (Element)
import UI.Button as Button
import UI.Internal.Size as Size
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text


checkbox : RenderConfig -> (Bool -> msg) -> String -> Bool -> Element msg
checkbox renderConfig message label state =
    Element.row [ Element.spacing 8 ]
        [ Button.toggle "Check"
            message
            state
            |> Button.withSize Size.extraSmall
            |> Button.renderElement renderConfig
        , Text.caption label
            |> Text.renderElement renderConfig
        ]