module UI.Radio exposing
    ( RadioGroup, RadioButton
    , group, button
    , withButtons, withSelected
    , RadioWidth, withWidth, widthFull, widthRelative
    , Direction, horizontal, vertical, withDirection
    , RadioSize, sizeSmall, sizeMedium, withSize
    , renderElement
    )

{-| Accessible and uniform-styled implementation of a radio buttons.

    Radio.group
        { label = "Pick a favorite animal:"
        , onSelectMsg = Msg.SelectRadio
        , idPrefix = "radio-animal"
        }
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


# Size

@docs RadioSize, sizeSmall, sizeMedium, withSize


# Rendering

@docs renderElement

-}

import Element exposing (Attribute, Element, fill, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html.Attributes as HtmlAttrs
import UI.Internal.Colors as Colors
import UI.Internal.SelectionControl as SelectionControl exposing (SelectionControlSize(..))
import UI.Internal.Utils.Element as Utils
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text


{-| The `RadioGroup option msg` type is used for describing the component for later rendering.
-}
type RadioGroup option msg
    = RadioGroup (Properties option msg) (Options option)


{-| The `RadioButton option` describes an individual radio-button
-}
type RadioButton option
    = RadioButton option String


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


{-| Describes the size of the radio buttons
-}
type RadioSize
    = RadioSize SelectionControl.SelectionControlSize


type alias Properties option msg =
    { label : String
    , onSelectMsg : String -> option -> msg
    , idPrefix : String
    }


type alias Options option =
    { selected : Maybe option
    , buttons : List (RadioButton option)
    , width : RadioWidth
    , direction : Direction
    , size : RadioSize
    }


{-| Starts an empty radio group.

    someRadioGroup =
        Radio.group
            { label = "Pick a card"
            , onSelectMsg = Msg.CardPicking
            , idPrefix = "card" -- Will result in "card-king-spades"
            }

-}
group :
    { label : String
    , onSelectMsg : String -> option -> msg
    , idPrefix : String
    }
    -> RadioGroup option msg
group props =
    RadioGroup props
        { selected = Nothing
        , buttons = []
        , width = WidthRelative
        , direction = Vertical
        , size = RadioSize SizeSM
        }


{-| A radio button and an element of a radio group.

    Radio.button Model.OrangeJuice "Orange Juice"

-}
button : option -> String -> RadioButton option
button option label =
    RadioButton option label


{-| Replaces a group's list of radio buttons.

    Radio.withButtons
        [ Radio.button Model.OrangeJuice "Orange Juice"
        , Radio.button Model.Lemonade "Lemonade"
        , Radio.button Model.SodaSoftDrink "Soda"
        ]
        someRadioGroup

-}
withButtons : List (RadioButton option) -> RadioGroup option msg -> RadioGroup option msg
withButtons buttons (RadioGroup prop opt) =
    RadioGroup prop { opt | buttons = buttons }


{-| Define one element as selected.

    Radio.withSelected (Just Model.DoubleCheddar)

-}
withSelected : Maybe option -> RadioGroup option msg -> RadioGroup option msg
withSelected maybeSelected (RadioGroup prop opt) =
    RadioGroup prop { opt | selected = maybeSelected }


{-| `Radio.withWidth` changes the width of the group.

    Radio.withWidth Radio.widthFull someRadioGroup

-}
withWidth : RadioWidth -> RadioGroup option msg -> RadioGroup option msg
withWidth width (RadioGroup prop opt) =
    RadioGroup prop { opt | width = width }


{-| `Radio.withDirection` determines whether the radio group's items are arranged horizontally or vertically.

    Radio.withDirection Radio.horizontal someRadioGroup

-}
withDirection : Direction -> RadioGroup option msg -> RadioGroup option msg
withDirection direction (RadioGroup prop opt) =
    RadioGroup prop { opt | direction = direction }


{-| `Radio.withSize` changes the size of the radio buttons

    Radio.withSize Radio.sizeMedium someRadioGroup

-}
withSize : RadioSize -> RadioGroup option msg -> RadioGroup option msg
withSize size (RadioGroup prop opt) =
    RadioGroup prop { opt | size = size }


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


{-| Small radio buttons (default value)
-}
sizeSmall : RadioSize
sizeSmall =
    RadioSize SizeSM


{-| Medium radio buttons
-}
sizeMedium : RadioSize
sizeMedium =
    RadioSize SizeMD


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> RadioGroup option msg -> Element msg
renderElement renderConfig (RadioGroup { label, onSelectMsg, idPrefix } { size, selected, buttons, width, direction }) =
    let
        radio =
            case direction of
                Vertical ->
                    Input.radio

                Horizontal ->
                    Input.radioRow
    in
    radio [ widthToEl width ]
        { onChange = \value -> onSelectMsg (findId idPrefix value buttons) value
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
                    , Element.htmlAttribute <| HtmlAttrs.tabindex -1
                    ]
        , options =
            List.indexedMap
                (\index ((RadioButton option _) as btn) ->
                    Input.optionWith
                        option
                        (renderButton renderConfig size (makeId idPrefix index) btn)
                )
                buttons
        }


makeId : String -> Int -> String
makeId idPrefix index =
    idPrefix ++ "-" ++ String.fromInt index


findId : String -> option -> List (RadioButton option) -> String
findId idPrefix value =
    List.indexedMap Tuple.pair
        >> List.foldl
            (\( index, RadioButton option _ ) acc ->
                if value == option then
                    Just <| makeId idPrefix index

                else
                    acc
            )
            Nothing
        >> Maybe.withDefault ""


optionStateToBool : Input.OptionState -> Bool
optionStateToBool state =
    case state of
        Input.Selected ->
            True

        _ ->
            False


renderButton : RenderConfig -> RadioSize -> String -> RadioButton option -> Input.OptionState -> Element msg
renderButton renderConfig (RadioSize size) id (RadioButton _ label) state =
    let
        isSelected =
            optionStateToBool state

        radioAttrs =
            Border.rounded 999
                :: SelectionControl.iconAttributes size
                    isSelected

        radioBulletContent =
            if isSelected then
                Element.el
                    [ Background.color <| SelectionControl.iconColor isSelected
                    , Element.width fill
                    , Element.height fill
                    , Border.color Colors.white
                    , Border.width 2
                    , Border.rounded 999
                    ]
                    Element.none

            else
                Element.none

        buttonAttributes =
            (Element.htmlAttribute <|
                HtmlAttrs.tabindex <|
                    if isSelected then
                        0

                    else
                        -1
            )
                :: Utils.id id
                :: SelectionControl.buttonAttributes size

        text =
            case size of
                SizeMD ->
                    Text.subtitle1

                SizeSM ->
                    Text.body2
    in
    Element.row
        buttonAttributes
        [ Element.el radioAttrs radioBulletContent
        , text label |> Text.renderElement renderConfig
        ]


widthToEl : RadioWidth -> Attribute msg
widthToEl width =
    case width of
        WidthFull ->
            Element.width fill

        WidthRelative ->
            Element.width shrink
