module UI.TextField exposing
    ( TextField
    , TextFieldContent
    , TextFieldWidth
    , currentPassword
    , email
    , multilineText
    , newPassword
    , renderElement
    , search
    , setLabelVisible
    , setPasswordVisible
    , singlelineText
    , spellChecked
    , static
    , username
    , widthFull
    , widthRelative
    , withError
    , withFocus
    , withIcon
    , withOnEnterPressed
    , withPlaceholder
    , withWidth
    )

{-
   Notes about decisions taken on the development process:
   * Every input must have a label value, even if hidden, for accessibility purposes.
   * Username, email, current password, and search activates in-browser autocomplete capabilities.
   * Username and Email content-types may have the same use case scenario (e.g., login, sign up), but the email has an in-browser mask checking.
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
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element
import UI.Utils.Focus exposing (Focus, focusAttributes)


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


type TextFieldWidth
    = WidthFull
    | WidthRelative



-- Options


withFocus : msg -> Int -> Bool -> TextField msg -> TextField msg
withFocus onEnter tabIndex hasFocus (TextField prop opt) =
    TextField prop
        { opt
            | focus = Just (Focus onEnter tabIndex hasFocus)
        }


withIcon : Icon -> TextField msg -> TextField msg
withIcon icon (TextField prop opt) =
    TextField prop
        { opt | icon = Just icon }


withPlaceholder : String -> TextField msg -> TextField msg
withPlaceholder placeholder (TextField prop opt) =
    TextField prop
        { opt | placeholder = placeholder }


withWidth : TextFieldWidth -> TextField msg -> TextField msg
withWidth width (TextField prop opt) =
    TextField prop
        { opt | width = width }


setLabelVisible : Bool -> TextField msg -> TextField msg
setLabelVisible isVisible (TextField prop opt) =
    TextField prop
        { opt | labelVisible = isVisible }


withError : String -> TextField msg -> TextField msg
withError caption (TextField prop opt) =
    TextField prop
        { opt | errorCaption = Just caption }


withOnEnterPressed : msg -> TextField msg -> TextField msg
withOnEnterPressed msg (TextField prop opt) =
    TextField prop
        { opt | onEnterPressed = Just msg }


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


widthFull : TextFieldWidth
widthFull =
    WidthFull


widthRelative : TextFieldWidth
widthRelative =
    WidthRelative



-- Constructors


singlelineText : (String -> msg) -> String -> String -> TextField msg
singlelineText onChange label currentValue =
    input onChange
        label
        currentValue
        ContentSinglelineText


multilineText : (String -> msg) -> String -> String -> TextField msg
multilineText onChange label currentValue =
    input onChange
        label
        currentValue
        ContentMultilineText


username : (String -> msg) -> String -> String -> TextField msg
username onChange label currentValue =
    input onChange
        label
        currentValue
        ContentUsername


newPassword : (String -> msg) -> String -> String -> TextField msg
newPassword onChange label currentValue =
    input onChange
        label
        currentValue
        forNewPassword


currentPassword : (String -> msg) -> String -> String -> TextField msg
currentPassword onChange label currentValue =
    input onChange
        label
        currentValue
        forCurrentPassword


email : (String -> msg) -> String -> String -> TextField msg
email onChange label currentValue =
    input onChange
        label
        currentValue
        ContentEmail


search : (String -> msg) -> String -> String -> TextField msg
search onChange label currentValue =
    input onChange
        label
        currentValue
        ContentSearch


spellChecked : (String -> msg) -> String -> String -> TextField msg
spellChecked onChange label currentValue =
    input onChange
        label
        currentValue
        ContentSpellChecked


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
                    focusAttributes details
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
    , ARIA.labelAttr label
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
