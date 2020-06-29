module UI.Utils.ARIA exposing
    ( Role, roleButton, roleImage, roleAttr
    , labelAttr
    )

{-| Interface for [HTML's ARIA](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA).


# Role

@docs Role, roleButton, roleImage, roleAttr


# Label

@docs labelAttr

-}

import Element exposing (Attribute)
import Html.Attributes as HtmlAttrs


{-| Roles defines the type of UI element.

See [MDN article](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques#Roles).

-}
type Role
    = RoleButton
    | RoleImage


{-| "The button role should be used for clickable elements that trigger a response when activated by the user." - MDN

    Element.el
        [ ARIA.roleAttr ARIA.roleButton
        , Events.onClick Msg.SomeEvent
        ]
        someChildElement

-}
roleButton : Role
roleButton =
    RoleButton


{-| "Can be used to identify multiple elements inside page content that should be considered as a single image." - MDN

    Element.el
        [ ARIA.roleAttr ARIA.roleImage
        ]
        [ Element.text "😺 Meow" ]

-}
roleImage : Role
roleImage =
    RoleImage


{-| Transform a [`Role`](#Role) in a valid [`Element.Attribute`](/packages/mdgriffith/elm-ui/latest/Element#Attribute)
-}
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
