module UI.Utils.ARIA exposing (Role, roleAttr, roleButton)

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
