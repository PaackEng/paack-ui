module UI.TextField exposing
    ( TextField
    , singlelineText, multilineText, spellChecked
    , newPassword, currentPassword, setPasswordVisible
    , username, email
    , search
    , static
    , TextFieldWidth, withWidth, widthFull, widthRelative
    , setLabelVisible, withPlaceholder, withIcon
    , withFocus, withOnEnterPressed, withError
    , renderElement
    )

{-| `UI.TextField` is an accessible and customizable interface for text inputs.
Indicating the type of input data unlocks specific features like mobile's autocompleting, browser's spell-checking, and password-related.

Different from [`Element.Input`](/packages/mdgriffith/elm-ui/latest/Element-Input), style is pre-applied following the design documents, and not customizable.

    TextField.email Msg.OnTextFieldChanged
        "Enter your email"
        model.emailValue
        |> TextField.setLabelVisible true
        |> TextField.renderElement renderConfig

**Notes**:

  - Every input must have a label value, even if hidden, for accessibility purposes.

  - Username, email, current password, and search activates in-browser autocomplete capabilities.

  - Username and email content-types may have the same use case scenario (e.g., login, sign up), but the email has an in-browser mask checking.


# Building

@docs TextField


## Text

@docs singlelineText, multilineText, spellChecked


## Password

@docs newPassword, currentPassword, setPasswordVisible


## Login

@docs username, email


## Search

@docs search


## Static

@docs static


# Width

@docs TextFieldWidth, withWidth, widthFull, widthRelative


# Accessibility

@docs setLabelVisible, withPlaceholder, withIcon


# Interactive

@docs withFocus, withOnEnterPressed, withError


# Rendering

@docs renderElement

-}

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import UI.Icon exposing (Icon)
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Internal.Text as Text
import UI.Palette as Palette exposing (brightnessMiddle, toneGray)
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.Element as Element
import UI.Utils.Focus as Focus exposing (Focus)


type alias Options msg =
    { placeholder : String
    , labelVisible : Bool
    , focus : Maybe (Focus msg)
    , icon : Maybe Icon
    , width : TextFieldWidth
    , errorCaption : Maybe String
    , onEnterPressed : Maybe msg
    }


type alias Properties msg =
    { changeable : Maybe (String -> msg)
    , label : String
    , content : TextFieldContent
    , currentValue : String
    }


{-| The `TextField msg` type is used for describing the component for later rendering.
-}
type TextField msg
    = TextField (Properties msg) (Options msg)


type TextFieldContent
    = ContentSinglelineText
    | ContentMultilineText
    | ContentUsername
    | ContentPassword PasswordOptions
    | ContentEmail
    | ContentSearch
    | ContentSpellChecked


type alias PasswordOptions =
    { isCurrent : Bool
    , isVisible : Bool
    }


{-| Describes a compatible width.
-}
type TextFieldWidth
    = WidthFull
    | WidthRelative



-- Options


{-| Listen to focus events, add tab-indexing and enforce focus state.

    TextField.withFocus
        { onEnter = Msg.FocusOnThisField
        , tabIndex = 1
        , hasFocus = True
        }
        someTextField

-}
withFocus : Focus msg -> TextField msg -> TextField msg
withFocus focusConfig (TextField prop opt) =
    TextField prop
        { opt
            | focus = Just focusConfig
        }


{-| Append an icon to the end of the text field.

    TextField.search Msg.OnTextFieldChanged
        "Search something"
        model.value
        |> TextField.withIcon
            (Icon.search "Search")

**NOTE**: Not ready.

-}
withIcon : Icon -> TextField msg -> TextField msg
withIcon icon (TextField prop opt) =
    TextField prop
        { opt | icon = Just icon }


{-| Place-holds the text field with text.

    TextField.withPlaceholder "Enter your personal email" someTextField

-}
withPlaceholder : String -> TextField msg -> TextField msg
withPlaceholder placeholder (TextField prop opt) =
    TextField prop
        { opt | placeholder = placeholder }


{-| `TextField.withWidth` changes the width of the field.

    TextField.withWidth TextField.widthFull someTextField

-}
withWidth : TextFieldWidth -> TextField msg -> TextField msg
withWidth width (TextField prop opt) =
    TextField prop
        { opt | width = width }


{-| Show or hide the text field's label.

    TextField.setLabelVisible True someTextField

-}
setLabelVisible : Bool -> TextField msg -> TextField msg
setLabelVisible isVisible (TextField prop opt) =
    TextField prop
        { opt | labelVisible = isVisible }


{-| Replaces the text with an error message and make the border red.

    TextField.withError "Minimum eight caracters." someTextField

**NOTE**: Not ready, just make border red by now.

-}
withError : String -> TextField msg -> TextField msg
withError caption (TextField prop opt) =
    TextField prop
        { opt | errorCaption = Just caption }


{-| Trigger message when the users press return-key while editing the text field.

    TextField.withOnEnterPressed Msg.SubmitField someTextField

-}
withOnEnterPressed : msg -> TextField msg -> TextField msg
withOnEnterPressed msg (TextField prop opt) =
    TextField prop
        { opt | onEnterPressed = Just msg }


{-| Make the password on [`newPassword`](#newPassword) and [`currentPassword`](#currentPassword) visible to the user.

    TextField.setPasswordVisible True someTextField

-}
setPasswordVisible : Bool -> TextField msg -> TextField msg
setPasswordVisible isVisible ((TextField prop opt) as original) =
    case prop.content of
        ContentPassword { isCurrent } ->
            TextField
                { prop
                    | content =
                        ContentPassword
                            { isCurrent = isCurrent
                            , isVisible = isVisible
                            }
                }
                opt

        _ ->
            original


{-| The field's width will fill its container.
-}
widthFull : TextFieldWidth
widthFull =
    WidthFull


{-| The field will have the exact width to fit its contents.

**NOTE**: Default behaviour.

-}
widthRelative : TextFieldWidth
widthRelative =
    WidthRelative



-- Constructors


{-| Wrapper around [`Element.Input.text` ](/packages/mdgriffith/elm-ui/latest/Element-Input#text).

    TextField.singlelineText Msg.OnTextFieldChanged
        "My cool input"
        model.value

-}
singlelineText : (String -> msg) -> String -> String -> TextField msg
singlelineText onChange label currentValue =
    input onChange
        label
        currentValue
        ContentSinglelineText


{-| Wrapper around [`Element.Input.multiline` ](/packages/mdgriffith/elm-ui/latest/Element-Input#multiline).

    TextField.multiline Msg.OnTextFieldChanged
        "My cool textarea"
        model.value

-}
multilineText : (String -> msg) -> String -> String -> TextField msg
multilineText onChange label currentValue =
    input onChange
        label
        currentValue
        ContentMultilineText


{-| Wrapper around [`Element.Input.username` ](/packages/mdgriffith/elm-ui/latest/Element-Input#username).

    TextField.username Msg.OnTextFieldChanged
        "Username"
        model.value

-}
username : (String -> msg) -> String -> String -> TextField msg
username onChange label currentValue =
    input onChange
        label
        currentValue
        ContentUsername


{-| Wrapper around [`Element.Input.newPassword` ](/packages/mdgriffith/elm-ui/latest/Element-Input#newPassword).

    TextField.newPassword Msg.OnTextFieldChanged
        "New password"
        model.value

-}
newPassword : (String -> msg) -> String -> String -> TextField msg
newPassword onChange label currentValue =
    input onChange
        label
        currentValue
        forNewPassword


{-| Wrapper around [`Element.Input.currentPassword` ](/packages/mdgriffith/elm-ui/latest/Element-Input#currentPassword).

    TextField.currentPassword Msg.OnTextFieldChanged
        "Current password"
        model.value

-}
currentPassword : (String -> msg) -> String -> String -> TextField msg
currentPassword onChange label currentValue =
    input onChange
        label
        currentValue
        forCurrentPassword


{-| Wrapper around [`Element.Input.email` ](/packages/mdgriffith/elm-ui/latest/Element-Input#email).

    TextField.email Msg.OnTextFieldChanged
        "Email"
        model.value
        |> TextField.setLabelVisible True

-}
email : (String -> msg) -> String -> String -> TextField msg
email onChange label currentValue =
    input onChange
        label
        currentValue
        ContentEmail


{-| Wrapper around [`Element.Input.search` ](/packages/mdgriffith/elm-ui/latest/Element-Input#search).

    TextField.search Msg.OnTextFieldChanged
        "Search something"
        model.value

-}
search : (String -> msg) -> String -> String -> TextField msg
search onChange label currentValue =
    input onChange
        label
        currentValue
        ContentSearch


{-| Wrapper around [`Element.Input.spellChecked` ](/packages/mdgriffith/elm-ui/latest/Element-Input#spellChecked).

    TextField.spellChecked Msg.OnTextFieldChanged
        "Spell checking"
        model.value

-}
spellChecked : (String -> msg) -> String -> String -> TextField msg
spellChecked onChange label currentValue =
    input onChange
        label
        currentValue
        ContentSpellChecked


{-| Simulate a [`TextField.singlelineText`](#singlelineText) visually, but the content isn't changeable.

    TextField.static
        "Not changeable"
        "Any constant value"

-}
static : String -> String -> TextField msg
static label value =
    TextField
        { changeable = Nothing
        , label = label
        , content = ContentMultilineText
        , currentValue = value
        }
        defaultOptions



-- Render


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> TextField msg -> Element msg
renderElement cfg (TextField prop opt) =
    let
        elAttrs =
            attrs cfg prop opt

        nonStatic content msg =
            case content of
                ContentSinglelineText ->
                    inputAnyOptions cfg msg prop opt
                        |> Input.text elAttrs

                ContentMultilineText ->
                    inputMultilineOptions cfg msg prop opt
                        |> Input.multiline elAttrs

                ContentUsername ->
                    inputAnyOptions cfg msg prop opt
                        |> Input.username elAttrs

                ContentPassword pswOpt ->
                    whenPassword msg pswOpt

                ContentEmail ->
                    inputAnyOptions cfg msg prop opt
                        |> Input.email elAttrs

                ContentSearch ->
                    inputAnyOptions cfg msg prop opt
                        |> Input.search elAttrs

                ContentSpellChecked ->
                    inputAnyOptions cfg msg prop opt
                        |> Input.spellChecked elAttrs

        whenStatic value =
            Text.subtitle2 value
                |> Text.renderElement cfg
                |> Element.el elAttrs

        whenPassword msg { isVisible, isCurrent } =
            if isCurrent then
                inputPasswordOptions cfg msg prop opt isVisible
                    |> Input.currentPassword elAttrs

            else
                inputPasswordOptions cfg msg prop opt isVisible
                    |> Input.newPassword elAttrs
    in
    case prop.changeable of
        Just msg ->
            nonStatic prop.content msg

        Nothing ->
            whenStatic prop.currentValue



-- Internals for contructors


forNewPassword : TextFieldContent
forNewPassword =
    ContentPassword
        { isCurrent = False
        , isVisible = False
        }


forCurrentPassword : TextFieldContent
forCurrentPassword =
    ContentPassword
        { isCurrent = True
        , isVisible = False
        }


input : (String -> msg) -> String -> String -> TextFieldContent -> TextField msg
input onChange label currentValue content =
    TextField
        { changeable = Just onChange
        , label = label
        , content = content
        , currentValue = currentValue
        }
        defaultOptions



-- Internals


defaultOptions : Options msg
defaultOptions =
    { placeholder = ""
    , labelVisible = False
    , focus = Nothing
    , icon = Nothing
    , width = WidthRelative
    , errorCaption = Nothing
    , onEnterPressed = Nothing
    }


inputAnyOptions :
    RenderConfig
    -> (String -> msg)
    -> Properties msg
    -> Options msg
    ->
        { label : Input.Label msg
        , onChange : String -> msg
        , placeholder : Maybe (Input.Placeholder msg)
        , text : String
        }
inputAnyOptions cfg onChange { label, currentValue } { placeholder, labelVisible } =
    { label = inputLabel cfg label labelVisible
    , onChange = onChange
    , placeholder =
        if placeholder /= "" then
            Element.text placeholder
                |> Input.placeholder []
                |> Just

        else
            Nothing
    , text = currentValue
    }


inputMultilineOptions :
    RenderConfig
    -> (String -> msg)
    -> Properties msg
    -> Options msg
    ->
        { label : Input.Label msg
        , onChange : String -> msg
        , placeholder : Maybe (Input.Placeholder msg)
        , text : String
        , spellcheck : Bool
        }
inputMultilineOptions cfg onChange { label, currentValue } { placeholder, labelVisible } =
    { label = inputLabel cfg label labelVisible
    , onChange = onChange
    , placeholder =
        if placeholder /= "" then
            Element.text placeholder
                |> Input.placeholder []
                |> Just

        else
            Nothing
    , spellcheck = True
    , text = currentValue
    }


inputPasswordOptions :
    RenderConfig
    -> (String -> msg)
    -> Properties msg
    -> Options msg
    -> Bool
    ->
        { label : Input.Label msg
        , onChange : String -> msg
        , placeholder : Maybe (Input.Placeholder msg)
        , text : String
        , show : Bool
        }
inputPasswordOptions cfg onChange { label, currentValue } { placeholder, labelVisible } isVisible =
    { label = inputLabel cfg label labelVisible
    , onChange = onChange
    , placeholder =
        if placeholder /= "" then
            Element.text placeholder
                |> Input.placeholder []
                |> Just

        else
            Nothing
    , show = isVisible
    , text = currentValue
    }


attrs : RenderConfig -> Properties msg -> Options msg -> List (Attribute msg)
attrs cfg prop opt =
    let
        hasError =
            opt.errorCaption /= Nothing

        isPlaceholder =
            prop.currentValue /= ""

        eventAttr acu =
            case opt.onEnterPressed of
                Just onEnterPressed ->
                    Element.onEnterPressed onEnterPressed
                        :: acu

                Nothing ->
                    acu

        focustAttr acu =
            case opt.focus of
                Just details ->
                    Focus.toElementAttributes details
                        ++ acu

                Nothing ->
                    acu
    in
    genericAttr prop.label
        isPlaceholder
        hasError
        opt.width
        |> eventAttr
        |> focustAttr
        |> (++) (textAttrs cfg)


textAttrs : RenderConfig -> List (Attribute msg)
textAttrs cfg =
    Text.attributes cfg Text.SizeSubtitle2 False Text.ColorInherit


genericAttr : String -> Bool -> Bool -> TextFieldWidth -> List (Attribute msg)
genericAttr label isPlaceholder hasError width =
    [ Background.color Palette.gray.lightest
    , Primitives.roundedFields
    , Border.color <|
        if hasError then
            Palette.danger.light

        else
            Palette.gray.lighter
    , Border.width <|
        if hasError then
            2

        else
            1
    , Element.paddingXY 18 16
    , Element.focused
        [ Border.color Palette.primary.lighter
        ]
    , Element.width <|
        case width of
            WidthFull ->
                Element.fill

            WidthRelative ->
                Element.shrink
    , Font.color <|
        -- TODO: Use CSS pre-processor
        if isPlaceholder then
            Palette.gray.darkest

        else
            Palette.gray.light
    , Element.title label
    ]


inputLabel : RenderConfig -> String -> Bool -> Input.Label msg
inputLabel cfg label labelVisible =
    if labelVisible then
        Text.caption label
            |> Text.withColor
                (Palette.color toneGray brightnessMiddle)
            |> Text.renderElement cfg
            |> Input.labelAbove
                [ Element.paddingEach
                    { top = 0, left = 0, right = 0, bottom = 3 }
                ]

    else
        Input.labelHidden label
