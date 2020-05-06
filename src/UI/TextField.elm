module UI.TextField exposing
    ( TextField
    , TextFieldContent
    , TextFieldWidth
    , forAny
    , forAnyMultiline
    , forEmail
    , forPassword
    , forSearch
    , forSpellChecked
    , forUsername
    , static
    , textField
    , toEl
    , widthFull
    , widthRelative
    , withError
    , withFocus
    , withIcon
    , withLabelVisible
    , withPlaceholder
    , withWidth
    )

import Element exposing (Attribute, Element)
import Element.Input as Input
import UI.Icon exposing (Icon)
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.Element exposing (Focus)


type alias Options msg =
    { placeHolder : String
    , labelVisible : Bool
    , focus : Maybe (Focus msg)
    , icon : Maybe Icon
    , width : TextFieldWidth
    , errorCaption : Maybe String
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
    = ContentAny
    | ContentAnyMultiline
    | ContentUsername
    | ContentPassword PasswordOptions
    | ContentEmail
    | ContentSearch
    | ContentSpellChecked


type alias PasswordOptions =
    { isCurrent : Bool
    , show : Bool
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
withPlaceholder placeHolder (TextField prop opt) =
    TextField prop
        { opt | placeHolder = placeHolder }


withWidth : TextFieldWidth -> TextField msg -> TextField msg
withWidth width (TextField prop opt) =
    TextField prop
        { opt | width = width }


withLabelVisible : Bool -> TextField msg -> TextField msg
withLabelVisible isVisible (TextField prop opt) =
    TextField prop
        { opt | labelVisible = isVisible }


withError : String -> TextField msg -> TextField msg
withError caption (TextField prop opt) =
    TextField prop
        { opt | errorCaption = Just caption }


forAny : TextFieldContent
forAny =
    ContentAny


forAnyMultiline : TextFieldContent
forAnyMultiline =
    ContentAnyMultiline


forUsername : TextFieldContent
forUsername =
    ContentUsername


forPassword : Bool -> Bool -> TextFieldContent
forPassword isCurrent show =
    PasswordOptions isCurrent show
        |> ContentPassword


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


textField : (String -> msg) -> String -> String -> TextFieldContent -> TextField msg
textField onChange label currentValue content =
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
        , content = ContentAnyMultiline
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

        ( Just msg, ContentAny ) ->
            inputAnyOptions msg prop opt
                |> Input.text
                    (attrs cfg prop opt)

        ( Just msg, ContentAnyMultiline ) ->
            inputMultilineOptions msg prop opt
                |> Input.multiline
                    (attrs cfg prop opt)

        ( Just msg, ContentUsername ) ->
            inputAnyOptions msg prop opt
                |> Input.username
                    (attrs cfg prop opt)

        ( Just msg, ContentPassword { show, isCurrent } ) ->
            if isCurrent then
                inputPasswordOptions msg prop opt
                    |> Input.currentPassword
                        (attrs cfg prop opt)

            else
                inputPasswordOptions msg prop opt
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
    { placeHolder = ""
    , labelVisible = True
    , focus = Nothing
    , icon = Nothing
    , width = WidthRelative
    , errorCaption = Nothing
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
inputAnyOptions onChange { label, currentValue } { placeHolder } =
    { label = Input.labelAbove [] (Element.text label)
    , onChange = onChange
    , placeholder =
        if placeHolder /= "" then
            Element.text placeHolder
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
inputMultilineOptions onChange { label, currentValue } { placeHolder } =
    { label = Input.labelAbove [] (Element.text label)
    , onChange = onChange
    , placeholder =
        if placeHolder /= "" then
            Element.text placeHolder
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
    ->
        { label : Input.Label msg
        , onChange : String -> msg
        , placeholder : Maybe (Input.Placeholder msg)
        , text : String
        , show : Bool
        }
inputPasswordOptions onChange { label, currentValue } { placeHolder } =
    { label = Input.labelAbove [] (Element.text label)
    , onChange = onChange
    , placeholder =
        if placeHolder /= "" then
            Element.text placeHolder
                |> Input.placeholder []
                |> Just

        else
            Nothing
    , show = True
    , text = currentValue
    }


attrs : RenderConfig -> Properties msg -> Options msg -> List (Attribute msg)
attrs cfg prop opt =
    []
