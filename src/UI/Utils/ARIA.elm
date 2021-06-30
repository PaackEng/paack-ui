module UI.Utils.ARIA exposing
    ( ElementSemantics
    , roleButton, roleImage, rolePresentation, roleCheckbox, roleTab, roleSwitch, roleToggleButton
    , roleRadioGroup, roleRadio
    , withLabel
    , toElementAttributes
    )

{-| Interface for [HTML's ARIA](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA).

@docs ElementSemantics


# Building

@docs roleButton, roleImage, rolePresentation, roleCheckbox, roleTab, roleSwitch, roleToggleButton


## Radio buttons

@docs roleRadioGroup, roleRadio


# Global options

@docs withLabel


# Rendering

@docs toElementAttributes

-}

import Element exposing (Attribute)
import Html.Attributes as HtmlAttrs
import UI.Internal.Basics exposing (ifThenElse, prependMaybe)


{-| Use roles for creating ARIA element's semantics.
Roles defines the type of UI element.

See [MDN article](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques#Roles).

-}
type alias ElementSemantics =
    { role : Role, label : Maybe String }


type Role
    = RoleButton
    | RoleCheckbox Bool
    | RoleSwitch Bool
    | RoleImage
    | RolePresentation
    | RoleRadio Bool
    | RoleRadioGroup
    | RoleTab Bool
    | RoleToggleButton Bool


{-| "The button role should be used for clickable elements that trigger a response when activated by the user." - MDN

    Element.el
        (Events.onClick Msg.SomeEvent :: ARIA.toElementAttributes ARIA.roleButton)
        someChildElement

-}
roleButton : ElementSemantics
roleButton =
    fromRole RoleButton


{-| A toggle button has the button role along with its pressed state

    Element.el
        (Events.onClick Msg.SomeEvent
            :: ARIA.toElementAttributes ARIA.roleToggleButton isToggled
        )
        someChildElement

-}
roleToggleButton : Bool -> ElementSemantics
roleToggleButton pressed =
    fromRole (RoleToggleButton pressed)


{-| "The checkbox role is used for checkable interactive controls." -MDN

    Element.row
        (ARIA.toElementAttributes <| ARIA.roleCheckbox False)
        [ notCheckedIcon
        , Element.text "I accept the Terms of Service"
        ]

-}
roleCheckbox : Bool -> ElementSemantics
roleCheckbox checked =
    fromRole (RoleCheckbox checked)


{-| "The ARIA switch role is functionally identical to the checkbox role,
except that instead of representing checked/unchecked states, which are fairly generic in meaning,
the switch role represents the states on/off." -MDN

    Element.el
        (Event.onClick turnOnMsg :: (ARIA.toElementAttributes <| ARIA.roleSwitch False))
        offSwitchIcon

-}
roleSwitch : Bool -> ElementSemantics
roleSwitch state =
    fromRole (RoleSwitch state)


{-| "Can be used to identify multiple elements inside page content that should be considered as a single image." - MDN

    Element.el
        (ARIA.toElementAttributes <| ARIA.roleImage altText)
        [ Element.text "😺 Meow" ]

**NOTE**: This role enforces aria-label option.

-}
roleImage : String -> ElementSemantics
roleImage label =
    { role = RoleImage, label = Just label }


{-| "An element whose content is completely presentational (like a spacer image, decorative graphic, or clearing element)" - W3C

    Element.el
        (ARIA.toElementAttributes ARIA.rolePresentation)
        totallyRedundantElement

-}
rolePresentation : ElementSemantics
rolePresentation =
    fromRole RolePresentation


{-| "A checkable input in a group of elements with the same role, only one of which can be checked at a time." - W3C

    Element.row
        (ARIA.toElementAttributes <| ARIA.roleRadio True)
        [ checkedIcon
        , Element.text "Chocolate"
        ]

-}
roleRadio : Bool -> ElementSemantics
roleRadio checked =
    fromRole (RoleRadio checked)


{-| "A radiogroup is a type of select list that can only have a single entry checked at any one time." - W3C

    Element.column
        (ARIA.toElementAttributes <| ARIA.roleRadioGroup "Pick an ice cream flavor")
        [ chocolateIceCream
        , strawberryIceCream
        ]

**NOTE**: This role enforces aria-label option.

-}
roleRadioGroup : String -> ElementSemantics
roleRadioGroup label =
    { role = RoleRadioGroup, label = Just label }


{-| "The ARIA tab role indicates an interactive element inside a tablist." -MDN

    Element.row []
        [ Element.el
            (ARIA.roleTab True
                |> ARIA.toElementAttributes
                |> (::) tabIndex 0
                |> (::) Event.onClick (Msg.SetTab TabFirst)
            )
            (Element.text "Tab label")
        , Element.el
            (ARIA.roleTab False
                |> ARIA.toElementAttributes
                |> (::) tabIndex -1
                |> (::) Event.onClick (Msg.SetTab TabSecond)
            )
            (Element.text "Another tab")
        ]

**NOTE**: We're missing `aria-controls`.
And MDN recomends using `tabindex` as `0` on selected tab and `-1` on non-active tabs.

-}
roleTab : Bool -> ElementSemantics
roleTab selected =
    fromRole (RoleTab selected)


{-| "Defines a string value that labels the current element" -W3C

    ARIA.roleCheckbox False
        |> ARIA.withLabel "I agree with the policy"
        |> ARIA.toElementAttributes

**NOTE**: This is a global optional parameter, roles builders enforce it when necessary.

-}
withLabel : String -> ElementSemantics -> ElementSemantics
withLabel label semantics =
    { semantics | label = Just label }


{-| Transform a [`ElementSemantics`](#ElementSemantics) in a list of [`Element.Attribute`](/packages/mdgriffith/elm-ui/latest/Element#Attribute).
-}
toElementAttributes : ElementSemantics -> List (Attribute msg)
toElementAttributes { role, label } =
    prependMaybe (Maybe.map labelAttr label) <|
        case role of
            RoleButton ->
                [ roleAttr "button"
                ]

            RoleToggleButton pressed ->
                [ roleAttr "button"
                , pressedAttr pressed
                ]

            RoleCheckbox checked ->
                [ roleAttr "checkbox"
                , checkedAttr checked
                ]

            RoleSwitch state ->
                [ roleAttr "checkbox"
                , checkedAttr state
                ]

            RoleImage ->
                [ roleAttr "img"
                ]

            RolePresentation ->
                [ roleAttr "presentation"
                ]

            RoleRadio checked ->
                [ roleAttr "radio"
                , checkedAttr checked
                ]

            RoleRadioGroup ->
                [ roleAttr "radiogroup"
                ]

            RoleTab selected ->
                [ roleAttr "tab"
                , selectedAttr selected
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


pressedAttr : Bool -> Attribute msg
pressedAttr value =
    ifThenElse value "true" "false"
        |> HtmlAttrs.attribute "aria-pressed"
        |> Element.htmlAttribute


checkedAttr : Bool -> Attribute msg
checkedAttr value =
    "false"
        |> ifThenElse value "true"
        |> HtmlAttrs.attribute "aria-checked"
        |> Element.htmlAttribute


fromRole : Role -> ElementSemantics
fromRole role =
    { role = role, label = Nothing }


selectedAttr : Bool -> Attribute msg
selectedAttr value =
    "false"
        |> ifThenElse value "true"
        |> HtmlAttrs.attribute "aria-selected"
        |> Element.htmlAttribute
