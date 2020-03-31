module UI.Input exposing
    ( Input
    , default
    , email
    , password
    , toEl
    , withFocus
    , withFullWidth
    , withLabel
    , withOnEnterPressed
    , withOnLoseFocus
    , withPlaceholder
    , withText
    )

import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input
import Element.Region as Region
import Helpers exposing (ifThenElse)
import UI.Attributes exposing (ariaChecked, ariaLabel, ariaRole, onClickWithoutPropagation, style, title)
import UI.Events exposing (onEnter)
import UI.Theme as Theme


type Input msg
    = Input (Options msg)


type InputType
    = Plain
    | Email
    | Password


type alias Options msg =
    { onChange : String -> msg
    , onEnter : Maybe msg
    , onLoseFocus : Maybe msg
    , isFocused : Bool
    , text : String
    , placeholder : Maybe String
    , label : String
    , inputType : InputType
    , isFullWidth : Bool
    }


defaultOptions : (String -> msg) -> Options msg
defaultOptions onChange =
    { onChange = onChange
    , onEnter = Nothing
    , onLoseFocus = Nothing
    , isFocused = False
    , text = ""
    , placeholder = Nothing
    , label = ""
    , inputType = Plain
    , isFullWidth = False
    }


default : (String -> msg) -> Input msg
default onChange =
    Input (defaultOptions onChange)


email : (String -> msg) -> Input msg
email onChange =
    let
        options =
            defaultOptions onChange
    in
    Input
        { options | inputType = Email }


password : (String -> msg) -> Input msg
password onChange =
    let
        options =
            defaultOptions onChange
    in
    Input
        { options | inputType = Password }


withText : String -> Input msg -> Input msg
withText text (Input options) =
    Input
        { options | text = text }


withPlaceholder : String -> Input msg -> Input msg
withPlaceholder placeholder (Input options) =
    Input
        { options | placeholder = Just placeholder }


withLabel : String -> Input msg -> Input msg
withLabel label (Input options) =
    Input
        { options | label = label }


withFullWidth : Input msg -> Input msg
withFullWidth (Input options) =
    Input
        { options | isFullWidth = True }


withFocus : Input msg -> Input msg
withFocus (Input options) =
    Input
        { options | isFocused = True }


withOnEnterPressed : msg -> Input msg -> Input msg
withOnEnterPressed msg (Input options) =
    Input
        { options | onEnter = Just msg }


withOnLoseFocus : msg -> Input msg -> Input msg
withOnLoseFocus msg (Input options) =
    Input
        { options | onLoseFocus = Just msg }


toEl : Input msg -> Element msg
toEl (Input options) =
    let
        inputWidth =
            width <| ifThenElse options.isFullWidth fill shrink

        labelAttributes =
            [ Theme.subtitle
            , Font.color Theme.gray2
            , paddingXY 0 Theme.sizings.tiny
            ]

        label =
            Element.Input.labelAbove labelAttributes (text options.label)

        placeholder =
            options.placeholder
                |> Maybe.map text
                |> Maybe.map (Element.Input.placeholder [ Font.color Theme.gray2 ])

        onEnterAttribute =
            case options.onEnter of
                Just onEnterMsg ->
                    [ onEnter onEnterMsg ]

                Nothing ->
                    []

        onLoseFocusAttribute =
            case options.onLoseFocus of
                Just onLoseFocusMsg ->
                    [ Events.onLoseFocus onLoseFocusMsg ]

                Nothing ->
                    []

        attributes =
            [ inputWidth
            , ariaLabel options.label
            , title options.label
            , Region.announce
            , Font.color Theme.gray1
            , Theme.roundedBorder
            , Background.color Theme.gray4
            ]
                |> List.append (ifThenElse options.isFocused [ Element.Input.focusedOnLoad ] [])
                |> List.append onEnterAttribute
                |> List.append onLoseFocusAttribute

        config =
            { onChange = options.onChange
            , text = options.text
            , placeholder = placeholder
            , label = label
            }
    in
    case options.inputType of
        Plain ->
            Element.Input.text attributes config

        Email ->
            Element.Input.email attributes config

        Password ->
            Element.Input.newPassword attributes
                { onChange = options.onChange
                , text = options.text
                , placeholder = placeholder
                , label = label
                , show = False
                }
