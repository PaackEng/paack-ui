module UI.Radio exposing
    ( RadioGroup, RadioButton
    , group, button
    , withButtons, withSelected
    , RadioWidth, withWidth, widthFull, widthRelative
    , Direction, horizontal, vertical, withDirection
    , renderElement
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


# Direction

@docs Direction, horizontal, vertical, withDirection


# Rendering

@docs renderElement

-}

import Element exposing (Attribute, Element, fill, px, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Html.Attributes as HtmlAttrs
import UI.Icon as Icon
import UI.Internal.Colors as Colors
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.ARIA as ARIA


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


{-| Describes the direction in which the radio group will be rendered.
-}
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


{-| `Radio.withDirection` determines whether the radio group's items are arranged horizontally or vertically.

    Radio.withDirection Radio.horizontal someRadioGroup

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
                    Input.radio

                Horizontal ->
                    Input.radioRow
    in
    layoutFunc [ widthToEl width ]
        { onChange = message
        , selected = selected
        , label =
            Text.body2 label
                |> Text.renderElement renderConfig
                |> Input.labelAbove
                    [ Element.paddingEach
                        { top = 0
                        , right = 0
                        , bottom = 8
                        , left = 0
                        }
                    ]
        , options =
            List.map
                (\(RadioButton id buttonLabel) ->
                    Input.optionWith
                        id
                        (renderButton renderConfig buttonLabel)
                )
                buttons
        }


optionStateToBool : Input.OptionState -> Bool
optionStateToBool state =
    case state of
        Input.Idle ->
            False

        Input.Focused ->
            False

        Input.Selected ->
            True


renderButton : RenderConfig -> String -> Input.OptionState -> Element msg
renderButton renderConfig label state =
    let
        isSelected =
            optionStateToBool state

        radioAttrs =
            Element.width (px 20)
                :: Element.height (px 20)
                :: Border.color Colors.primary.middle
                :: Border.width 2
                :: Border.rounded 999
                :: (ARIA.toElementAttributes <| ARIA.rolePresentation)

        radioIcon =
            if isSelected then
                Element.el
                    radioAttrs
                    (radioCheck renderConfig)

            else
                Element.el
                    radioAttrs
                    Element.none

        rowAttrs =
            [ Element.spacing 8
            , Element.width fill
            , Element.padding 8
            , Element.pointer
            , Border.rounded 6
            , Element.mouseOver [ Background.color <| Colors.gray.light3 ]
            , Element.htmlAttribute <| HtmlAttrs.tabindex 0
            , Element.focused
                [ Border.innerShadow { offset = ( 0, 0 ), size = 2, blur = 0, color = Colors.primary.middle }
                ]
            ]
    in
    Element.row rowAttrs
        [ radioIcon
        , Text.caption label
            |> Text.renderElement renderConfig
        ]


radioCheck : RenderConfig -> Element msg
radioCheck renderConfig =
    Element.el
        [ Background.color Colors.primary.middle
        , Element.width (px 12)
        , Element.height (px 12)
        , Element.centerY
        , Element.centerX
        , Border.rounded 999
        ]
        Element.none


widthToEl : RadioWidth -> Attribute msg
widthToEl width =
    case width of
        WidthFull ->
            Element.width fill

        WidthRelative ->
            Element.width shrink
