module UI.Button exposing
    ( Button
    , button
    , link
    , toEl
    , toggle
    , withDangerColor
    , withDisabledMode
    , withFullWidth
    , withIcon
    , withPrimaryColor
    , withSuccessColor
    , withText
    , withWarningColor
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Helpers exposing (ifThenElse)
import UI.Attributes as Attributes
import UI.Theme as Theme


type Content msg
    = Text String
    | Custom (Element msg)


type ButtonType msg
    = Normal msg
    | Link String
    | Toggle (Bool -> msg) Bool


type alias Options msg =
    { colorScheme : ColorScheme
    , content : Content msg
    , buttonType : ButtonType msg
    , isDisabled : Bool
    , isFullWidth : Bool
    }


type Button msg
    = Button (Options msg)


type alias ColorScheme =
    { normal : ColorConfig
    , hover : ColorConfig
    }


type alias ColorConfig =
    { bg : Color
    , text : Color
    , border : Color
    }


defaultOptions : Options msg
defaultOptions =
    { content = Text ""
    , colorScheme = disabledColorScheme
    , buttonType = Link ""
    , isDisabled = False
    , isFullWidth = False
    }


button : msg -> Button msg
button msg =
    Button
        { defaultOptions
            | buttonType = Normal msg
        }


toggle : (Bool -> msg) -> Bool -> Button msg
toggle msg isEnabled =
    Button
        { defaultOptions
            | buttonType = Toggle msg isEnabled
        }


link : String -> Button msg
link url =
    Button
        { defaultOptions
            | buttonType = Link url
        }


withDisabledMode : Bool -> Button msg -> Button msg
withDisabledMode isDisabled (Button options) =
    Button
        { options
            | isDisabled = isDisabled
        }


disabledColorScheme : ColorScheme
disabledColorScheme =
    let
        default =
            { bg = Theme.gray4
            , text = Theme.gray2
            , border = Theme.gray2
            }
    in
    { normal = default
    , hover = default
    }


withIcon : Element msg -> Button msg -> Button msg
withIcon content (Button options) =
    Button
        { options
            | content = Custom content
        }


withPrimaryColor : Button msg -> Button msg
withPrimaryColor (Button options) =
    Button
        { options
            | colorScheme =
                { normal =
                    { bg = Theme.primary
                    , text = Theme.white
                    , border = Theme.primary
                    }
                , hover =
                    { bg = Theme.white
                    , text = Theme.primary
                    , border = Theme.primary
                    }
                }
        }


withSuccessColor : Button msg -> Button msg
withSuccessColor (Button options) =
    Button
        { options
            | colorScheme =
                { normal =
                    { bg = Theme.success
                    , text = Theme.black
                    , border = Theme.success
                    }
                , hover =
                    { bg = Theme.white
                    , text = Theme.black
                    , border = Theme.success
                    }
                }
        }


withDangerColor : Button msg -> Button msg
withDangerColor (Button options) =
    Button
        { options
            | colorScheme =
                { normal =
                    { bg = Theme.error
                    , text = Theme.white
                    , border = Theme.error
                    }
                , hover =
                    { bg = Theme.white
                    , text = Theme.error
                    , border = Theme.error
                    }
                }
        }


withWarningColor : Button msg -> Button msg
withWarningColor (Button options) =
    Button
        { options
            | colorScheme =
                { normal =
                    { bg = Theme.warning
                    , text = Theme.black
                    , border = Theme.warning
                    }
                , hover =
                    { bg = Theme.white
                    , text = Theme.black
                    , border = Theme.warning
                    }
                }
        }


withFullWidth : Button msg -> Button msg
withFullWidth (Button options) =
    Button
        { options | isFullWidth = True }


withText : String -> Button msg -> Button msg
withText val (Button options) =
    Button
        { options
            | content = Text val
        }


toEl : Button msg -> Element msg
toEl (Button options) =
    let
        baseAttrs =
            case options.buttonType of
                Link _ ->
                    []

                _ ->
                    [ Font.size 16
                    , Theme.roundedBorder
                    , Border.width 1
                    , Font.center
                    , buttonWidth
                    , buttonPadding
                    , Attributes.ariaRole "button"
                    ]

        buttonPadding =
            case options.content of
                Custom _ ->
                    paddingXY 12 8

                Text _ ->
                    paddingXY 30 10

        buttonWidth =
            width <| ifThenElse options.isFullWidth fill shrink

        disabledAttrs =
            if options.isDisabled then
                [ Attributes.custom "disabled" "true"
                , Attributes.custom "aria-disabled" "true"
                , Attributes.custom "tabindex" "-1"
                , Attributes.custom "pointer-events" "none"
                , Attributes.custom "cursor" "default"
                ]

            else
                [ pointer ]

        clickAttrs =
            case options.buttonType of
                Normal msg ->
                    [ Events.onClick msg ]

                Toggle msg isEnabled ->
                    [ Events.onClick (msg (not isEnabled)) ]

                Link _ ->
                    []

        attrs =
            baseAttrs
                ++ colorAttrs options
                ++ disabledAttrs
                ++ ifThenElse options.isDisabled [] clickAttrs
    in
    case options.buttonType of
        Link url ->
            Element.link
                attrs
                { url = url
                , label = elFromContent options.content
                }

        _ ->
            el attrs <|
                elFromContent options.content


colorAttrs : Options msg -> List (Attr () msg)
colorAttrs { colorScheme, buttonType, isDisabled } =
    let
        attrs normal hover =
            case buttonType of
                Link _ ->
                    [ Font.color hover.text
                    , Font.underline
                    , mouseOver
                        [ Font.color Theme.black
                        ]
                    ]

                _ ->
                    [ Background.color normal.bg
                    , Font.color normal.text
                    , mouseOver
                        [ Background.color hover.bg
                        , Font.color hover.text
                        , Border.color hover.border
                        ]
                    , Border.color normal.border
                    , Attributes.transition 100 [ "color", "background-color" ]
                    ]

        disabledMode =
            attrs disabledColorScheme.normal disabledColorScheme.hover

        enabledMode =
            case buttonType of
                Normal _ ->
                    attrs colorScheme.normal colorScheme.hover

                Link _ ->
                    attrs colorScheme.normal colorScheme.hover

                Toggle _ isEnabled ->
                    if isEnabled then
                        attrs colorScheme.normal colorScheme.hover

                    else
                        attrs colorScheme.hover colorScheme.normal
    in
    ifThenElse isDisabled disabledMode enabledMode


elFromContent : Content msg -> Element msg
elFromContent content =
    case content of
        Text str ->
            text str

        Custom el ->
            el
