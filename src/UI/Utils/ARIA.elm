module UI.Utils.ARIA exposing (Role, labelAttr, roleAttr, roleButton)

import Element exposing (Attribute)
import Html.Attributes as HtmlAttrs


type Role
    = RoleButton


roleButton : Role
roleButton =
    RoleButton


roleAttr : Role -> Attribute msg
roleAttr role =
    Element.htmlAttribute <|
        HtmlAttrs.attribute "role" <|
            case role of
                RoleButton ->
                    "button"


labelAttr : String -> Attribute msg
labelAttr value =
    value
        |> HtmlAttrs.attribute "aria-label"
        |> Element.htmlAttribute
