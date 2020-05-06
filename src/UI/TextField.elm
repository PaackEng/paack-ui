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

import Element exposing (Element)
import UI.Icon exposing (Icon)
import UI.RenderConfig exposing (RenderConfig)
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


static : Bool -> String -> String -> TextField msg
static isMultiline label value =
    TextField
        { changeable = Nothing
        , label = label
        , content =
            if isMultiline then
                ContentAnyMultiline

            else
                ContentAny
        , currentValue = value
        }
        defaultOptions



-- Render


toEl : RenderConfig -> TextField msg -> Element msg
toEl cfg (TextField prop opt) =
    Element.none



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
