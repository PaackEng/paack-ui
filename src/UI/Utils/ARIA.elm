module UI.Utils.ARIA exposing
    ( ElementSemantics, roleButton, roleImage, rolePresentation
    , roleRadioGroup, roleRadio
    , toElementAttributes
    )

{-| Interface for [HTML's ARIA](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA).


# Building

@docs ElementSemantics, roleButton, roleImage, rolePresentation


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
    | RoleImage String
    | RoleRadioGroup String
    | RoleRadio Bool
    | RolePresentation


{-| "The button role should be used for clickable elements that trigger a response when activated by the user." - MDN

    Element.el
        (Events.onClick Msg.SomeEvent :: (ARIA.toElementAttributes <| ARIA.roleButton))
        someChildElement

-}
roleButton : ElementSemantics
roleButton =
    RoleButton


{-| "Can be used to identify multiple elements inside page content that should be considered as a single image." - MDN

    Element.el
        (ARIA.toElementAttributes <| ARIA.roleImage altText)
        [ Element.text "😺 Meow" ]

-}
roleImage : String -> ElementSemantics
roleImage label =
    RoleImage label


roleRadioGroup : String -> ElementSemantics
roleRadioGroup label =
    RoleRadioGroup label


roleRadio : Bool -> ElementSemantics
roleRadio checked =
    RoleRadio checked


rolePresentation : ElementSemantics
rolePresentation =
    RolePresentation


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

        RoleImage label ->
            [ roleAttr "img"
            , labelAttr label
            ]

        RoleRadioGroup label ->
            [ roleAttr "radiogroup"
            , labelAttr label
            ]

        RoleRadio checked ->
            [ roleAttr "radio"
            , checkedAttr checked
            ]

        RolePresentation ->
            [ roleAttr "presentation"
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
