module UI.Utils.ARIA exposing (Role, labelAttr, roleAttr, roleButton, roleImage)

import Element exposing (Attribute)
import Html.Attributes as HtmlAttrs


type Role
    = RoleButton
    | RoleImage


roleButton : Role
roleButton =
    RoleButton


roleImage : Role
roleImage =
    RoleImage


roleAttr : Role -> Attribute msg
roleAttr role =
    Element.htmlAttribute <|
        HtmlAttrs.attribute "role" <|
            case role of
                RoleButton ->
                    "button"

                RoleImage ->
                    "img"


labelAttr : String -> Attribute msg
labelAttr value =
    value
        |> HtmlAttrs.attribute "aria-label"
        |> Element.htmlAttribute
