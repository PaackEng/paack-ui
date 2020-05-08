module UI.TextField exposing
    ( TextField
    , TextFieldContent
    , TextFieldWidth
    , forCurrentPassword
    , forEmail
    , forMultilineText
    , forNewPassword
    , forSearch
    , forSinglelineText
    , forSpellChecked
    , forUsername
    , static
    , toEl
    , widthFull
    , widthRelative
    , withError
    , withFocus
    , withIcon
    , withInput
    , withLabelNotHidden
    , withOnEnterPressed
    , withPasswordNotHidden
    , withPlaceholder
    , withWidth
    )

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import UI.Icon exposing (Icon)
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
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


withLabelNotHidden : Bool -> TextField msg -> TextField msg
withLabelNotHidden isVisible (TextField prop opt) =
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


withPasswordNotHidden : Bool -> TextField msg -> TextField msg
withPasswordNotHidden isVisible ((TextField prop opt) as original) =
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


forSinglelineText : TextFieldContent
forSinglelineText =
    ContentSinglelineText


forMultilineText : TextFieldContent
forMultilineText =
    ContentMultilineText


forUsername : TextFieldContent
forUsername =
    ContentUsername


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


forEmail : TextFieldContent
forEmail =
    ContentEmail


forSearch : TextFieldContent
forSearch =
    ContentSearch


forSpellChecked : TextFieldContent
forSpellChecked =
    ContentSpellChecked


widthFull : TextFieldWidth
widthFull =
    WidthFull


widthRelative : TextFieldWidth
widthRelative =
    WidthRelative



-- Constructors


withInput : (String -> msg) -> String -> String -> TextFieldContent -> TextField msg
withInput onChange label currentValue content =
    TextField
        { changeable = Just onChange
        , label = label
        , content = content
        , currentValue = currentValue
        }
        defaultOptions


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


toEl : RenderConfig -> TextField msg -> Element msg
toEl cfg (TextField prop opt) =
    case ( prop.changeable, prop.content ) of
        ( Nothing, _ ) ->
            Text.body1 prop.currentValue
                |> Text.toEl cfg

        ( Just msg, ContentSinglelineText ) ->
            inputAnyOptions msg prop opt
                |> Input.text
                    (attrs cfg prop opt)

        ( Just msg, ContentMultilineText ) ->
            inputMultilineOptions msg prop opt
                |> Input.multiline
                    (attrs cfg prop opt)

        ( Just msg, ContentUsername ) ->
            inputAnyOptions msg prop opt
                |> Input.username
                    (attrs cfg prop opt)

        ( Just msg, ContentPassword { isVisible, isCurrent } ) ->
            if isCurrent then
                inputPasswordOptions msg prop opt isVisible
                    |> Input.currentPassword
                        (attrs cfg prop opt)

            else
                inputPasswordOptions msg prop opt isVisible
                    |> Input.newPassword
                        (attrs cfg prop opt)

        ( Just msg, ContentEmail ) ->
            inputAnyOptions msg prop opt
                |> Input.email
                    (attrs cfg prop opt)

        ( Just msg, ContentSearch ) ->
            inputAnyOptions msg prop opt
                |> Input.search
                    (attrs cfg prop opt)

        ( Just msg, ContentSpellChecked ) ->
            inputAnyOptions msg prop opt
                |> Input.spellChecked
                    (attrs cfg prop opt)



-- Internals


defaultOptions : Options msg
defaultOptions =
    { placeholder = ""
    , labelVisible = True
    , focus = Nothing
    , icon = Nothing
    , width = WidthRelative
    , errorCaption = Nothing
    , onEnterPressed = Nothing
    }


inputAnyOptions :
    (String -> msg)
    -> Properties msg
    -> Options msg
    ->
        { label : Input.Label msg
        , onChange : String -> msg
        , placeholder : Maybe (Input.Placeholder msg)
        , text : String
        }
inputAnyOptions onChange { label, currentValue } { placeholder, labelVisible } =
    { label =
        if labelVisible then
            Input.labelAbove [] (Element.text label)

        else
            Input.labelHidden label
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
    (String -> msg)
    -> Properties msg
    -> Options msg
    ->
        { label : Input.Label msg
        , onChange : String -> msg
        , placeholder : Maybe (Input.Placeholder msg)
        , text : String
        , spellcheck : Bool
        }
inputMultilineOptions onChange { label, currentValue } { placeholder, labelVisible } =
    { label =
        if labelVisible then
            Input.labelAbove [] (Element.text label)

        else
            Input.labelHidden label
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
    (String -> msg)
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
inputPasswordOptions onChange { label, currentValue } { placeholder, labelVisible } isVisible =
    { label =
        if labelVisible then
            Input.labelAbove [] (Element.text label)

        else
            Input.labelHidden label
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
    , Font.size 14
    , Font.bold
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
