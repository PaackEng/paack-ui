module UI.Internal.Checkbox exposing (checkbox, radioButton)

import Element exposing (Element)
import UI.RenderConfig exposing (RenderConfig)


checkbox : RenderConfig -> (Bool -> msg) -> String -> Bool -> Element msg
checkbox _ _ _ _ =
    Element.none


radioButton : RenderConfig -> (Bool -> msg) -> String -> Bool -> Element msg
radioButton _ _ _ _ =
    Element.none
