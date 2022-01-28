module UI.Button exposing
    ( Button, toggle, disabled, cmd, redirect
    , ButtonBody, fromLabel, fromIcon
    , fromLabeledOnLeftIcon, fromLabeledOnRightIcon
    , ButtonStyle, hyperlink, primary, danger, light, clear, switchedOn
    , ButtonWidth, withWidth, widthFull, widthRelative
    , withSize
    , withDisabledIf
    , withId
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

@docs Button, toggle, disabled, cmd, redirect


# Content

@docs ButtonBody, fromLabel, fromIcon
@docs fromLabeledOnLeftIcon, fromLabeledOnRightIcon


# Style

@docs ButtonStyle, hyperlink, primary, danger, light, clear, switchedOn


# Width

@docs ButtonWidth, withWidth, widthFull, widthRelative


# Size

@docs withSize


# Conditional states

@docs withDisabledIf


# Identification

@docs withId


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
import UI.Internal.Utils.Element as ElementUtils
import UI.Link as Link exposing (Link)
import UI.Palette as Palette
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
    , id = Nothing
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


{-| `Button.fromLabeledOnLeftIcon` initiates a button's body with icon-content and it's label on the left.

    Button.fromLabeledOnLeftIcon (Icon.map "Go to maps")

-}
fromLabeledOnLeftIcon : Icon -> ButtonBody
fromLabeledOnLeftIcon icon =
    BodyIconLeftText icon


{-| `Button.fromLabeledOnRightIcon` initiates a button's body with icon-content and it's label on the right.

    Button.fromLabeledOnRightIcon (Icon.map "Go to maps")

-}
fromLabeledOnRightIcon : Icon -> ButtonBody
fromLabeledOnRightIcon icon =
    BodyIconRightText icon



-- Options


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


{-| With `Button.withId`, you can add an [HTML ID attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/id) to the button element.

    Button.withId (Just "id") someButton

-}
withId : Maybe String -> Button msg -> Button msg
withId id button =
    case button of
        Button prop opt ->
            Button prop { opt | id = id }

        Toggle prop opt ->
            Toggle prop { opt | id = id }



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


{-| The switched on state for a button. Any active button can be in a a possible "On" state.
-}
switchedOn : ButtonStyle
switchedOn =
    StyleEmbossed ToneSwitchedOn


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
        Toggle { hint, current, toggleMsg } options ->
            toggleView cfg hint toggleMsg current options

        Button { mode, body } options ->
            case mode of
                ButtonActive action StyleHyperlink ->
                    hyperlinkView cfg body action options

                ButtonActive action (StyleEmbossed tone) ->
                    workingView cfg tone body action options

                ButtonDisabled ->
                    staticView cfg body disabledTheme options



-- Modes' renders


toggleView :
    RenderConfig
    -> String
    -> (Bool -> msg)
    -> Bool
    -> Options
    -> Element msg
toggleView cfg hint toggleMsg current { size, id } =
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
        |> Element.el (prependMaybe (Maybe.map ElementUtils.id id) attrs)


hyperlinkView :
    RenderConfig
    -> ButtonBody
    -> ButtonAction msg
    -> Options
    -> Element msg
hyperlinkView cfg body action { width, id, size } =
    let
        attrs =
            buttonWidth width
                :: (Palette.blue700
                        |> Palette.toElementColor
                        |> Font.color
                   )
                :: Font.regular
                :: Font.underline
                :: Element.pointer
                :: (ARIA.toElementAttributes <| ARIA.roleButton)

        attrsWithId =
            prependMaybe (Maybe.map ElementUtils.id id) attrs
    in
    case action of
        ActionRedirect link ->
            body
                |> bodyToElement cfg size
                |> Link.wrapElement cfg attrsWithId link

        ActionMsg msg ->
            body
                |> bodyToElement cfg size
                |> Element.el (Element.onIndividualClick msg :: attrsWithId)


workingView :
    RenderConfig
    -> EmbossedTone
    -> ButtonBody
    -> ButtonAction msg
    -> Options
    -> Element msg
workingView cfg tone body action { size, width, id } =
    let
        attrs =
            Primitives.roundedBorders size
                :: buttonWidth width
                :: Font.semiBold
                :: Element.pointer
                :: (ARIA.toElementAttributes <| ARIA.roleButton)
                ++ workingTheme tone
                ++ bodyAttrs body size

        attrsWithId =
            prependMaybe (Maybe.map ElementUtils.id id) attrs
    in
    case action of
        ActionRedirect link ->
            body
                |> bodyToElement cfg size
                |> Link.wrapElement cfg attrsWithId link

        ActionMsg msg ->
            body
                |> bodyToElement cfg size
                |> Element.el (Element.onIndividualClick msg :: attrsWithId)


staticView :
    RenderConfig
    -> ButtonBody
    -> List (Attribute msg)
    -> Options
    -> Element msg
staticView cfg body theme { size, width, id } =
    let
        attrs =
            Primitives.roundedBorders size
                :: buttonWidth width
                :: Font.semiBold
                :: Element.disabled
                ++ bodyAttrs body size
                ++ theme
    in
    body
        |> bodyToElement cfg size
        |> Element.el (prependMaybe (Maybe.map ElementUtils.id id) attrs)



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

        BodyIconLeftText _ ->
            companionLayout size

        BodyIconRightText _ ->
            companionLayout size


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


companionLayout : Size -> List (Attribute msg)
companionLayout size =
    let
        border =
            borderWidth size

        paddingXY =
            case size of
                Size.Large ->
                    ( 40 - border, ((60 - 28) // 2) - border )

                Size.Medium ->
                    ( 32 - border, ((48 - 20) // 2) - border )

                Size.Small ->
                    ( 20 - border, ((36 - 16) // 2) - border )

                Size.ExtraSmall ->
                    ( 12 - border, ((24 - 12) // 2) - border )
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
                { background = Palette.genericWhite
                , border = Palette.blue700
                , text =
                    Palette.blue700
                        |> Text.ColorPalette
                }
            , hover =
                Just
                    { background = Palette.blue200
                    , border = Palette.blue800
                    , text =
                        Palette.blue800
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
                    { background = Palette.red700
                    , border = Palette.red700
                    , text = Text.ColorPalette Palette.genericWhite
                    }
                , hover =
                    Just
                        { background = Palette.red800
                        , border = Palette.red800
                        , text = Text.ColorPalette Palette.genericWhite
                        }
                }

            ToneLight ->
                { normal =
                    { background = Palette.gray200
                    , border = Palette.gray200
                    , text =
                        Palette.blue700
                            |> Text.ColorPalette
                    }
                , hover =
                    Just
                        { background = Palette.gray300
                        , border = Palette.gray300
                        , text = Text.ColorPalette Palette.blue800
                        }
                }

            ToneClear ->
                { normal =
                    { background =
                        Palette.genericWhite
                            |> Palette.withAlpha 0
                    , border =
                        Palette.genericWhite
                            |> Palette.withAlpha 0
                    , text = Text.ColorPalette Palette.blue700
                    }
                , hover =
                    Just
                        { background = Palette.gray200
                        , border = Palette.gray200
                        , text = Text.ColorPalette Palette.blue700
                        }
                }

            ToneSwitchedOn ->
                switchedOnTheme


disabledTheme : List (Attribute msg)
disabledTheme =
    themeToAttributes <|
        { normal =
            { background = Palette.gray300
            , border = Palette.gray300
            , text = Text.ColorPalette Palette.genericWhite
            }
        , hover = Nothing
        }


switchedOnTheme : ButtonTheme
switchedOnTheme =
    { normal =
        { background = Palette.blue200
        , border = Palette.blue200
        , text = Text.ColorPalette Palette.blue700
        }
    , hover =
        Just
            { background = Palette.blue300
            , border = Palette.blue300
            , text = Text.ColorPalette Palette.blue800
            }
    }


primaryTheme : ButtonTheme
primaryTheme =
    { normal =
        { background = Palette.blue700
        , border = Palette.blue700
        , text = Text.ColorPalette Palette.genericWhite
        }
    , hover =
        Just
            { background = Palette.blue800
            , border = Palette.blue800
            , text = Text.ColorPalette Palette.genericWhite
            }
    }
