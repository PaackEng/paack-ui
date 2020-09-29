module UI.Radio exposing
    ( RadioGroup, RadioButton
    , group, button
    , withButtons, withSelected
    , RadioWidth, withWidth, widthFull, widthRelative
    , renderElement
    , Direction, horizontal, vertical, withDirection
    )

{-| Accessible and uniform-styled implementation of a radio buttons.

    Radio.group
        "Pick a favorite animal:"
        Msg.SelectRadio
        |> Radio.withSelected (Just Model.Felines)
        |> Radio.withButtons
            [ Radio.button Model.Felines "Felines"
            , Radio.button Model.Canines "Canines"
            , Radio.button Model.Birds "Birds"
            ]
        |> Radio.renderElement renderConfig


# Types

@docs RadioGroup, RadioButton


# Constructors

@docs group, button


# Group management

@docs withButtons, withSelected


# Width

@docs RadioWidth, withWidth, widthFull, widthRelative


# Rendering

@docs renderElement

-}

import Element exposing (Attribute, Element, fill, px, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import UI.Icon as Icon
import UI.Internal.Palette as Palette
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element


{-| The `RadioGroup id msg` type is used for describing the component for later rendering.
-}
type RadioGroup id msg
    = RadioGroup (Properties id msg) (Options id)


{-| The `RadioButton id` describes an individual radiobutton
-}
type RadioButton id
    = RadioButton id String


{-| Describes a compatible width.
-}
type RadioWidth
    = WidthFull
    | WidthRelative


type Direction
    = Vertical
    | Horizontal


type alias Properties id msg =
    { label : String
    , message : id -> msg
    }


type alias Options id =
    { selected : Maybe id
    , buttons : List (RadioButton id)
    , width : RadioWidth
    , direction : Direction
    }


{-| Starts an empty radio group.
The first argument is the label used for accessibility.
The second is the message triggered when there is a selection.

    someRadioGroup =
        Radio.group "Pick a card" Msg.CardPicking

-}
group : String -> (id -> msg) -> RadioGroup id msg
group label message =
    RadioGroup { label = label, message = message }
        { selected = Nothing, buttons = [], width = WidthRelative, direction = Vertical }


{-| A radio button and an element of a radio group.

    Radio.button Model.OrangeJuice "Orange Juice"

-}
button : id -> String -> RadioButton id
button id label =
    RadioButton id label


{-| Replaces a group's list of radio buttons.

    Radio.withButtons
        [ Radio.button Model.OrangeJuice "Orange Juice"
        , Radio.button Model.Lemonade "Lemonade"
        , Radio.button Model.SodaSoftDrink "Soda"
        ]
        someRadioGroup

-}
withButtons : List (RadioButton id) -> RadioGroup id msg -> RadioGroup id msg
withButtons buttons (RadioGroup prop opt) =
    RadioGroup prop { opt | buttons = buttons }


{-| Define one element as selected.

    Radio.withSelected (Just Model.DoubleCheddar)

-}
withSelected : Maybe id -> RadioGroup id msg -> RadioGroup id msg
withSelected maybeSelected (RadioGroup prop opt) =
    RadioGroup prop { opt | selected = maybeSelected }


{-| `Radio.withWidth` changes the width of the group.

    Radio.withWidth Radio.widthFull someRadioGroup

-}
withWidth : RadioWidth -> RadioGroup id msg -> RadioGroup id msg
withWidth width (RadioGroup prop opt) =
    RadioGroup prop { opt | width = width }


{-| `Radio.withDisposition` determines whether the radio group's items are arranged horizontally or vertically.

    Radio.withDisposition Radio.horizontalDisposition someRadioGroup

-}
withDirection : Direction -> RadioGroup id msg -> RadioGroup id msg
withDirection direction (RadioGroup prop opt) =
    RadioGroup prop { opt | direction = direction }


{-| When displaying, arrange the buttons in horizontal lines.
-}
horizontal : Direction
horizontal =
    Horizontal


{-| When displaying, arrange the buttons in a column.
-}
vertical : Direction
vertical =
    Vertical


{-| All the radio buttons' width will fill the container.
-}
widthFull : RadioWidth
widthFull =
    WidthFull


{-| The buttons will have the exact width to fit its contents.

**NOTE**: This is the default value.

-}
widthRelative : RadioWidth
widthRelative =
    WidthRelative


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> RadioGroup id msg -> Element msg
renderElement renderConfig (RadioGroup { label, message } { selected, buttons, width, direction }) =
    let
        layoutFunc =
            case direction of
                Vertical ->
                    Element.column

                Horizontal ->
                    Element.wrappedRow
    in
    buttons
        |> List.map
            (\(RadioButton id buttonLabel) ->
                renderButton renderConfig
                    (message id)
                    buttonLabel
                    (selected == Just id)
            )
        |> layoutFunc
            (widthToEl width
                :: (ARIA.toElementAttributes <| ARIA.roleRadioGroup label)
            )


renderButton : RenderConfig -> msg -> String -> Bool -> Element msg
renderButton renderConfig message label state =
    let
        radioAttrs =
            Element.width (px 20)
                :: Element.height (px 20)
                :: Border.color Palette.primary.middle
                :: Border.width 2
                :: Border.rounded 8
                :: (ARIA.toElementAttributes <| ARIA.rolePresentation)

        radioIcon =
            if state then
                Element.el
                    (radioSelected :: radioAttrs)
                    (radioCheck renderConfig)

            else
                Element.el
                    radioAttrs
                    Element.none

        rowAttrs =
            Element.spacing 8
                :: Element.width fill
                :: Element.paddingXY 12 4
                :: Events.onClick message
                :: Element.pointer
                :: Element.mouseOver [ Background.color <| Palette.gray.lightest ]
                :: (ARIA.toElementAttributes <| ARIA.roleRadio state)
    in
    Element.row rowAttrs
        [ radioIcon
        , Text.caption label
            |> Text.renderElement renderConfig
        ]


radioSelected : Attribute msg
radioSelected =
    Background.color Palette.primary.middle


radioCheck : RenderConfig -> Element msg
radioCheck renderConfig =
    (renderConfig |> localeTerms >> .radio >> .select)
        |> Icon.check
        |> Icon.withCustomSize 14
        |> Icon.withColor
            (Palette.color
                Palette.tonePrimary
                Palette.brightnessMiddle
                |> Palette.setContrasting True
            )
        |> Icon.renderElement renderConfig
        |> Element.el
            [ Element.centerY
            , Element.centerX
            ]


widthToEl : RadioWidth -> Attribute msg
widthToEl width =
    case width of
        WidthFull ->
            Element.width fill

        WidthRelative ->
            Element.width shrink
