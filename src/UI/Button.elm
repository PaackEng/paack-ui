module UI.Button exposing
    ( Button, toggle, success, disabled, cmd, redirect
    , ButtonBody, fromLabel, fromIcon
    , ButtonStyle, hyperlink, primary, danger, light, clear
    , ButtonWidth, withWidth, widthFull, widthRelative
    , withSize
    , withDisabledIf, withSuccessIf
    , renderElement
    , map
    )

{-| The `UI.Button` is a component that can render as a hyperlink, a togglable button, a stylized button, or a clear padded-icon.

Following Elm-UI standards, this component is accessible.

A button can be created and rendered as in the following pipeline:

    Element.column []
        [ -- Some UI.TextFields (...)
        , Button.fromLabel "Submit"
            |> Button.cmd FormSend Button.primary
            |> Button.renderElement renderConfig
        ]


# Building

@docs Button, toggle, success, disabled, cmd, redirect


# Content

@docs ButtonBody, fromLabel, fromIcon


# Style

@docs ButtonStyle, hyperlink, primary, danger, light, clear


# Width

@docs ButtonWidth, withWidth, widthFull, widthRelative


# Size

@docs withSize


# Conditional states

@docs withDisabledIf, withSuccessIf


# Rendering

@docs renderElement


# Component handling

@docs map

-}

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Basics exposing (lazyMap, pairUncurry, prependMaybe)
import UI.Internal.Button as Internal
    exposing
        ( Button(..)
        , ButtonAction(..)
        , ButtonBody(..)
        , ButtonMode(..)
        , ButtonStyle(..)
        , ButtonWidth(..)
        , EmbossedTone(..)
        , bodyToElement
        )
import UI.Internal.Primitives as Primitives
import UI.Internal.Size as Size exposing (Size)
import UI.Internal.Text as Text exposing (TextColor)
import UI.Link as Link exposing (Link)
import UI.Palette as Palette exposing (brightnessDarkest, brightnessLight, brightnessLighter, brightnessLightest, brightnessMiddle, tonePrimary)
import UI.RenderConfig exposing (RenderConfig)
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element


type alias Options =
    Internal.Options


{-| The `Button msg` type is used for describing the component for later rendering.
-}
type alias Button msg =
    Internal.Button msg


{-| The `ButtonBody` is required when assembling the most-basic `Button msg` type.
It indicates the contents inside of the desired button, like its label or icon.
-}
type alias ButtonBody =
    Internal.ButtonBody


type alias ButtonAction msg =
    Internal.ButtonAction msg


type alias EmbossedTone =
    Internal.EmbossedTone


{-| Non-toggle buttons must-be styled. The currently available styles are Hyperlink and Embossed.

A hyperlink-styled: See [`Button.hyperlink`](#hyperlink).

An embossed-styled button has paddings and hovering-effects.
It's available through its sub-themes: Primary, Danger, Light, and Clear.
These only change background and text color.

-}
type alias ButtonStyle =
    Internal.ButtonStyle


{-| Describes a compatible width.
-}
type alias ButtonWidth =
    Internal.ButtonWidth



-- Default


defaultOptions : Options
defaultOptions =
    { width = WidthShrink
    , size = Size.default
    }



-- Builders


{-| Toggle is a kind-of button that always contains a toggle-icon and visually looks like an embossed-primary button.
The purpose of this button is to toggle between showing/hiding some spammed-content.

    Button.toggle "Some Hint for Accessibility" TheTogglingMessage model.isSomethingVisible

-}
toggle : String -> (Bool -> msg) -> Bool -> Button msg
toggle hint msg isEnabled =
    Toggle { toggleMsg = msg, current = isEnabled, hint = hint } defaultOptions


{-| This `Button.disabled` builds an embossed-looking, without-message, grayish-colored button.
It's another approach for [`Button.withDisabledIf`](#withDisabledIf), helping when you can't compose a message for the desired action at the occasion.

    case event of
        Just id ->
            Button.cmd (TriggerEvent id) Button.primary body

        Nothing ->
            Button.disabled body

-}
disabled : ButtonBody -> Button msg
disabled body =
    Button { mode = ButtonDisabled, body = body } defaultOptions


{-| This `Button.success` builds an embossed-looking, without-message, greenish-colored button.
It's another approach for [`Button.withSuccessIf`](#withSuccessIf), helping when you can't compose a message for the desired action at the occasion.

    case event of
        Just id ->
            Button.cmd (TriggerEvent id) Button.primary body

        Nothing ->
            Button.success body

-}
success : ButtonBody -> Button msg
success body =
    Button { mode = ButtonSuccess, body = body } defaultOptions


{-| This is the most common builder.
It uses a simple message that is triggered on a click and renders as an embossed and themed button.

    Button.fromLabel "Click this Button"
        |> Button.cmd SomeSimpleMessage Button.primary
        |> Button.renderElement renderConfig

-}
cmd : msg -> ButtonStyle -> ButtonBody -> Button msg
cmd msg style body =
    Button
        { mode = ButtonActive (ActionMsg msg) style
        , body = body
        }
        defaultOptions


{-| Similar to [`Button.cmd`](#cmd), but instead of a message, it redirects to some path.

    Button.fromLabel "Click this Link"
        |> Button.redirect "https://elm-lang.org/" Button.hyperlink
        |> Button.renderElement renderConfig

-}
redirect : Link -> ButtonStyle -> ButtonBody -> Button msg
redirect link style body =
    Button
        { mode = ButtonActive (ActionRedirect link) style
        , body = body
        }
        defaultOptions



-- Body builders


{-| `Button.fromLabel` initiates a button's body with text-content inside it.

    Button.fromLabel "Click here"

-}
fromLabel : String -> ButtonBody
fromLabel label =
    BodyText label


{-| `Button.fromIcon` initiates a button's body with icon-content inside it.

    Button.fromIcon (Icon.map "Go to maps")

-}
fromIcon : Icon -> ButtonBody
fromIcon icon =
    BodyIcon icon



-- Options


{-| After asserting some condition, `Button.withSuccessIf` will attempt to set the button to a visually-noticeable success state (a greenish button where the action can no longer be triggered).

    Button.fromLabel "Send Someting"
        |> Button.cmd (QuerySend "Something") Button.primary
        |> Button.withSuccessIf (model.queryResult == QueryOkay)
        |> Button.renderElement renderConfig

**NOTE**: In case the button is a toggle or the condition resolves as False, nothing will happen.

-}
withSuccessIf : Bool -> Button msg -> Button msg
withSuccessIf condition button =
    if condition then
        case button of
            Toggle _ _ ->
                button

            Button { body } opt ->
                Button { mode = ButtonSuccess, body = body } opt

    else
        button


{-| After asserting some condition, `Button.withDisabledIf` will attempt to set the button to a visually-noticeable disabled state (a grayish button where the action can no longer be triggered).

    Button.fromLabel "Send Someting"
        |> Button.cmd (QuerySend "Something") Button.primary
        |> Button.withDisabledIf (model.queryResult != QueryNotAsked)
        |> Button.renderElement renderConfig

**NOTE**: In case the button is a toggle or the condition resolves as False, nothing will happen.

-}
withDisabledIf : Bool -> Button msg -> Button msg
withDisabledIf condition button =
    if condition then
        case button of
            Toggle _ _ ->
                button

            Button { body } opt ->
                Button { mode = ButtonDisabled, body = body } opt

    else
        button


{-| `Button.withWidth` changes the width of the button.

    Button.withWidth Button.widthFull someButton

-}
withWidth : ButtonWidth -> Button msg -> Button msg
withWidth width button =
    case button of
        Toggle prop opt ->
            Toggle prop { opt | width = width }

        Button prop opt ->
            Button prop { opt | width = width }


{-| With `Button.withSize`, you'll be able to scale the button between the [standard sizes][size].

[size]: UI-Size

The sizes (in height) are: Large - 60px; Medium - 48px; Small - 36px; Extra Small - 24px.

    Button.withSize Size.large someButton

**NOTE**: Button's default size is [`Size.medium`](UI-Size#medium)

-}
withSize : Size -> Button msg -> Button msg
withSize size button =
    case button of
        Toggle prop opt ->
            Toggle prop { opt | size = size }

        Button prop opt ->
            Button prop { opt | size = size }



-- Style


{-| This is the danger-theme, and it's reddish for enforcing the user's attention.
-}
danger : ButtonStyle
danger =
    StyleEmbossed ToneDanger


{-| This is the light-theme, mostly used for less-important actions.
-}
light : ButtonStyle
light =
    StyleEmbossed ToneLight


{-| This is the clear-theme, mostly used on icons where the background color isn't needed.
-}
clear : ButtonStyle
clear =
    StyleEmbossed ToneClear


{-| The primary action's theme.
This button usually commits the main task.
-}
primary : ButtonStyle
primary =
    StyleEmbossed TonePrimary


{-| A hyperlink-styled button looks like classical web links: Blue with an underline.
-}
hyperlink : ButtonStyle
hyperlink =
    StyleHyperlink



-- Width


{-| The button's width will fill its container.
-}
widthFull : ButtonWidth
widthFull =
    WidthFull


{-| The button will have the exact width to fit its contents.

**NOTE**: Default behaviour.

-}
widthRelative : ButtonWidth
widthRelative =
    WidthShrink



-- Component Handling


{-| Transform the messages produced by the component.
-}
map : (a -> b) -> Button a -> Button b
map applier button =
    let
        newAction oldAction =
            case oldAction of
                ActionMsg msg ->
                    ActionMsg (applier msg)

                ActionRedirect realLink ->
                    ActionRedirect realLink
    in
    case button of
        Button { mode, body } opt ->
            case mode of
                ButtonActive action style ->
                    Button { mode = ButtonActive (newAction action) style, body = body } opt

                ButtonDisabled ->
                    Button { mode = ButtonDisabled, body = body } opt

                ButtonSuccess ->
                    Button { mode = ButtonSuccess, body = body } opt

        Toggle { current, toggleMsg, hint } opt ->
            Toggle
                { current = current
                , toggleMsg = lazyMap applier toggleMsg
                , hint = hint
                }
                opt



-- Render


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Button msg -> Element msg
renderElement cfg button =
    case button of
        Toggle { hint, current, toggleMsg } { size } ->
            toggleView cfg size hint toggleMsg current

        Button { mode, body } { size, width } ->
            case mode of
                ButtonActive action StyleHyperlink ->
                    hyperlinkView cfg size width body action

                ButtonActive action (StyleEmbossed tone) ->
                    workingView cfg size width tone body action

                ButtonDisabled ->
                    staticView cfg size width body disabledTheme

                ButtonSuccess ->
                    staticView cfg size width body successTheme



-- Modes' renders


toggleView :
    RenderConfig
    -> Size
    -> String
    -> (Bool -> msg)
    -> Bool
    -> Element msg
toggleView cfg size hint toggleMsg current =
    let
        attrs =
            Primitives.roundedBorders size
                :: (Element.onIndividualClick <| toggleMsg (not current))
                :: (ARIA.toElementAttributes <| ARIA.roleToggleButton current)
                ++ toggleTheme current
                ++ iconLayout hint size
    in
    Icon.toggle hint
        |> fromIcon
        |> bodyToElement cfg size
        |> Element.el attrs


hyperlinkView :
    RenderConfig
    -> Size
    -> ButtonWidth
    -> ButtonBody
    -> ButtonAction msg
    -> Element msg
hyperlinkView cfg size width body action =
    let
        attrs =
            buttonWidth width
                :: (Palette.color tonePrimary brightnessMiddle
                        |> Palette.toElementColor
                        |> Font.color
                   )
                :: Font.regular
                :: Font.underline
                :: Element.pointer
                :: (ARIA.toElementAttributes <| ARIA.roleButton)
    in
    case action of
        ActionRedirect link ->
            body
                |> bodyToElement cfg size
                |> Link.wrapElement cfg attrs link

        ActionMsg msg ->
            body
                |> bodyToElement cfg size
                |> Element.el (Element.onIndividualClick msg :: attrs)


workingView :
    RenderConfig
    -> Size
    -> ButtonWidth
    -> EmbossedTone
    -> ButtonBody
    -> ButtonAction msg
    -> Element msg
workingView cfg size width tone body action =
    let
        attrs =
            Primitives.roundedBorders size
                :: buttonWidth width
                :: Font.semiBold
                :: Element.pointer
                :: (ARIA.toElementAttributes <| ARIA.roleButton)
                ++ workingTheme tone
                ++ bodyAttrs body size
    in
    case action of
        ActionRedirect link ->
            body
                |> bodyToElement cfg size
                |> Link.wrapElement cfg attrs link

        ActionMsg msg ->
            body
                |> bodyToElement cfg size
                |> Element.el (Element.onIndividualClick msg :: attrs)


staticView :
    RenderConfig
    -> Size
    -> ButtonWidth
    -> ButtonBody
    -> (ButtonBody -> List (Attribute msg))
    -> Element msg
staticView cfg size width body theme =
    let
        attrs =
            Primitives.roundedBorders size
                :: buttonWidth width
                :: Font.semiBold
                :: Element.disabled
                ++ bodyAttrs body size
                ++ theme body
    in
    body
        |> bodyToElement cfg size
        |> Element.el attrs



-- Attributes


buttonWidth : ButtonWidth -> Attribute msg
buttonWidth width =
    case width of
        WidthFull ->
            Element.width Element.fill

        WidthShrink ->
            Element.width Element.shrink


bodyAttrs : ButtonBody -> Size -> List (Attribute msg)
bodyAttrs body size =
    case body of
        BodyText _ ->
            textLayout size

        BodyIcon icon ->
            iconLayout (Icon.getHint icon) size


iconLayout : String -> Size -> List (Attribute msg)
iconLayout hint size =
    let
        border =
            borderWidth size

        paddingXY =
            case size of
                Size.Large ->
                    ( 10 - border, 10 - border )

                Size.Medium ->
                    ( 8 - border, 8 - border )

                Size.Small ->
                    ( 6 - border, 6 - border )

                Size.ExtraSmall ->
                    ( 4 - border, 4 - border )
    in
    [ pairUncurry Element.paddingXY paddingXY
    , Border.width border
    , Element.title hint
    ]


textLayout : Size -> List (Attribute msg)
textLayout size =
    let
        border =
            borderWidth size

        paddingXY =
            case size of
                Size.Large ->
                    ( 40 - border, ((60 - 20) // 2) - border )

                Size.Medium ->
                    ( 32 - border, ((48 - 16) // 2) - border )

                Size.Small ->
                    ( 20 - border, ((36 - 12) // 2) - border )

                Size.ExtraSmall ->
                    ( 12 - border, ((24 - 10) // 2) - border )
    in
    [ pairUncurry Element.paddingXY paddingXY
    , Border.width border
    ]


borderWidth : Size -> Int
borderWidth size =
    case size of
        Size.Large ->
            3

        Size.Medium ->
            3

        Size.Small ->
            2

        Size.ExtraSmall ->
            1



-- Theme applier


type alias ThemeTriple =
    { background : Palette.Color
    , border : Palette.Color
    , text : TextColor
    }


type alias ButtonTheme =
    { normal : ThemeTriple
    , hover : Maybe ThemeTriple
    }


themeToAttributes : ButtonTheme -> List (Attribute msg)
themeToAttributes theme =
    case theme.hover of
        Just hoverTriple ->
            (hoverTriple
                |> tripleToAttributes
                |> Element.mouseOver
            )
                :: Element.colorTransition 100
                ++ tripleToAttributes theme.normal

        Nothing ->
            tripleToAttributes theme.normal


tripleToAttributes : ThemeTriple -> List (Element.Attr decorative msg)
tripleToAttributes { background, border, text } =
    [ Background.color (Palette.toElementColor background)
    , Border.color (Palette.toElementColor border)
    ]
        |> prependMaybe (Maybe.map Font.color (Text.fontColor text))



-- Themes


toggleTheme : Bool -> List (Attribute msg)
toggleTheme current =
    themeToAttributes <|
        if current then
            primaryTheme

        else
            { normal =
                { background =
                    Palette.color Palette.tonePrimary brightnessMiddle
                        |> Palette.setContrasting True
                , border = Palette.color Palette.tonePrimary brightnessMiddle
                , text =
                    Palette.color Palette.tonePrimary brightnessMiddle
                        |> Text.ColorPalette
                }
            , hover =
                Just
                    { background = Palette.color Palette.tonePrimary brightnessLightest
                    , border = Palette.color Palette.tonePrimary brightnessDarkest
                    , text =
                        Palette.color Palette.tonePrimary brightnessDarkest
                            |> Text.ColorPalette
                    }
            }


workingTheme : EmbossedTone -> List (Attribute msg)
workingTheme tone =
    themeToAttributes <|
        case tone of
            TonePrimary ->
                primaryTheme

            ToneDanger ->
                { normal =
                    { background = Palette.color Palette.toneDanger brightnessMiddle
                    , border = Palette.color Palette.toneDanger brightnessMiddle
                    , text =
                        Palette.color Palette.toneDanger brightnessMiddle
                            |> Palette.setContrasting True
                            |> Text.ColorPalette
                    }
                , hover =
                    Just
                        { background = Palette.color Palette.toneDanger brightnessDarkest
                        , border = Palette.color Palette.toneDanger brightnessDarkest
                        , text =
                            Palette.color Palette.toneDanger brightnessDarkest
                                |> Palette.setContrasting True
                                |> Text.ColorPalette
                        }
                }

            ToneLight ->
                { normal =
                    { background = Palette.color Palette.toneGray brightnessLightest
                    , border = Palette.color Palette.toneGray brightnessLightest
                    , text =
                        Palette.color Palette.tonePrimary brightnessMiddle
                            |> Text.ColorPalette
                    }
                , hover =
                    Just
                        { background = Palette.color Palette.toneGray brightnessLighter
                        , border = Palette.color Palette.toneGray brightnessLighter
                        , text =
                            Palette.color Palette.tonePrimary brightnessDarkest
                                |> Text.ColorPalette
                        }
                }

            ToneClear ->
                { normal =
                    { background =
                        Palette.color Palette.tonePrimary brightnessMiddle
                            |> Palette.withAlpha 0
                    , border =
                        Palette.color Palette.tonePrimary brightnessMiddle
                            |> Palette.withAlpha 0
                    , text =
                        Palette.color Palette.tonePrimary brightnessMiddle
                            |> Text.ColorPalette
                    }
                , hover =
                    Just
                        { background = Palette.color Palette.toneGray brightnessLightest
                        , border = Palette.color Palette.toneGray brightnessLightest
                        , text =
                            Palette.color Palette.tonePrimary brightnessMiddle
                                |> Text.ColorPalette
                        }
                }


disabledTheme : ButtonBody -> List (Attribute msg)
disabledTheme body =
    themeToAttributes <|
        case body of
            BodyIcon _ ->
                { normal =
                    { background = Palette.color Palette.toneGray brightnessLightest
                    , border = Palette.color Palette.toneGray brightnessLightest
                    , text =
                        Palette.color Palette.toneGray brightnessLight
                            |> Text.ColorPalette
                    }
                , hover = Nothing
                }

            BodyText _ ->
                { normal =
                    { background = Palette.color Palette.toneGray brightnessLight
                    , border = Palette.color Palette.toneGray brightnessLight
                    , text =
                        Palette.color Palette.toneGray brightnessLight
                            |> Palette.setContrasting True
                            |> Text.ColorPalette
                    }
                , hover = Nothing
                }


successTheme : ButtonBody -> List (Attribute msg)
successTheme _ =
    themeToAttributes <|
        { normal =
            { background = Palette.color Palette.toneSuccess brightnessMiddle
            , border = Palette.color Palette.toneSuccess brightnessMiddle
            , text =
                Palette.color Palette.toneSuccess brightnessMiddle
                    |> Palette.setContrasting True
                    |> Text.ColorPalette
            }
        , hover =
            Just
                { background = Palette.color Palette.toneSuccess brightnessDarkest
                , border = Palette.color Palette.toneSuccess brightnessDarkest
                , text =
                    Palette.color Palette.toneSuccess brightnessDarkest
                        |> Palette.setContrasting True
                        |> Text.ColorPalette
                }
        }


primaryTheme : ButtonTheme
primaryTheme =
    { normal =
        { background = Palette.color Palette.tonePrimary brightnessMiddle
        , border = Palette.color Palette.tonePrimary brightnessMiddle
        , text =
            Palette.color Palette.tonePrimary brightnessMiddle
                |> Palette.setContrasting True
                |> Text.ColorPalette
        }
    , hover =
        Just
            { background = Palette.color Palette.tonePrimary brightnessDarkest
            , border = Palette.color Palette.tonePrimary brightnessDarkest
            , text =
                Palette.color Palette.tonePrimary brightnessDarkest
                    |> Palette.setContrasting True
                    |> Text.ColorPalette
            }
    }
