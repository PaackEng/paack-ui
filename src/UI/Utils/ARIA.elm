module UI.Utils.ARIA exposing
    ( ElementSemantics, roleButton, roleImage, rolePresentation, roleCheckbox
    , roleRadioGroup, roleRadio
    , toElementAttributes
    )

{-| Interface for [HTML's ARIA](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA).


# Building

@docs ElementSemantics, roleButton, roleImage, rolePresentation, roleCheckbox


## Radio buttons

@docs roleRadioGroup, roleRadio


# Rendering

@docs toElementAttributes

-}

import Element exposing (Attribute)
import Html.Attributes as HtmlAttrs
import UI.Internal.Basics exposing (ifThenElse)


{-| Roles defines the type of UI element.

See [MDN article](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques#Roles).

-}
type ElementSemantics
    = RoleButton
    | RoleCheckbox Bool
    | RoleImage String
    | RolePresentation
    | RoleRadio Bool
    | RoleRadioGroup String


{-| "The button role should be used for clickable elements that trigger a response when activated by the user." - MDN

    Element.el
        (Events.onClick Msg.SomeEvent :: (ARIA.toElementAttributes <| ARIA.roleButton))
        someChildElement

-}
roleButton : ElementSemantics
roleButton =
    RoleButton


roleCheckbox : Bool -> ElementSemantics
roleCheckbox checked =
    RoleCheckbox checked


{-| "Can be used to identify multiple elements inside page content that should be considered as a single image." - MDN

    Element.el
        (ARIA.toElementAttributes <| ARIA.roleImage altText)
        [ Element.text "😺 Meow" ]

-}
roleImage : String -> ElementSemantics
roleImage label =
    RoleImage label


{-| "An element whose content is completely presentational (like a spacer image, decorative graphic, or clearing element)" - W3C

    Element.el
        (ARIA.toElementAttributes <| ARIA.rolePresentation)
        totallyRedundantElement

-}
rolePresentation : ElementSemantics
rolePresentation =
    RolePresentation


{-| "A checkable input in a group of elements with the same role, only one of which can be checked at a time." - W3C

    Element.row
        (ARIA.toElementAttributes <| ARIA.roleRadio True)
        [ checkedIcon
        , Element.text "Chocolate"
        ]

-}
roleRadio : Bool -> ElementSemantics
roleRadio checked =
    RoleRadio checked


{-| "A radiogroup is a type of select list that can only have a single entry checked at any one time." - W3C

    Element.column
        (ARIA.toElementAttributes <| ARIA.roleRadioGroup "Pick an ice cream flavor")
        [ chocolateIceCream
        , strawberryIceCream
        ]

-}
roleRadioGroup : String -> ElementSemantics
roleRadioGroup label =
    RoleRadioGroup label


{-| Transform a [`ElementSemantics`](#ElementSemantics) in a list of [`Element.Attribute`](/packages/mdgriffith/elm-ui/latest/Element#Attribute).
-}
toElementAttributes : ElementSemantics -> List (Attribute msg)
toElementAttributes role =
    case role of
        RoleButton ->
            [ roleAttr "button"
            , pressedAttr "false"
            , expandedAttr "undefined"
            ]

        RoleCheckbox checked ->
            [ roleAttr "checkbox"
            , checkedAttr checked
            ]

        RoleImage label ->
            [ roleAttr "img"
            , labelAttr label
            ]

        RolePresentation ->
            [ roleAttr "presentation"
            ]

        RoleRadio checked ->
            [ roleAttr "radio"
            , checkedAttr checked
            ]

        RoleRadioGroup label ->
            [ roleAttr "radiogroup"
            , labelAttr label
            ]


roleAttr : String -> Attribute msg
roleAttr role =
    role
        |> HtmlAttrs.attribute "role"
        |> Element.htmlAttribute


labelAttr : String -> Attribute msg
labelAttr value =
    value
        |> HtmlAttrs.attribute "aria-label"
        |> Element.htmlAttribute


pressedAttr : String -> Attribute msg
pressedAttr value =
    value
        |> HtmlAttrs.attribute "aria-pressed"
        |> Element.htmlAttribute


expandedAttr : String -> Attribute msg
expandedAttr value =
    value
        |> HtmlAttrs.attribute "aria-expanded"
        |> Element.htmlAttribute


checkedAttr : Bool -> Attribute msg
checkedAttr value =
    "false"
        |> ifThenElse value "true"
        |> HtmlAttrs.attribute "aria-checked"
        |> Element.htmlAttribute
