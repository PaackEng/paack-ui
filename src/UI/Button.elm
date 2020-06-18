module UI.Button exposing
    ( Button
    , ButtonBody
    , ButtonStyle
    , ButtonWidth
    , clear
    , cmd
    , danger
    , disabled
    , fromIcon
    , fromLabel
    , full
    , hyperlink
    , light
    , map
    , primary
    , redirect
    , renderElement
    , shrink
    , success
    , toggle
    , withDisabledIf
    , withSize
    , withSuccessIf
    , withWidth
    )

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Basics exposing (lazyMap, maybeToList, pairUncurry, prependMaybe)
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Internal.Size as Size exposing (Size)
import UI.Internal.Text as Text exposing (TextColor)
import UI.Link as Link exposing (Link)
import UI.Palette as Palette exposing (brightnessDarkest, brightnessLight, brightnessLighter, brightnessLightest, brightnessMiddle, tonePrimary)
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element


type alias Options =
    { width : ButtonWidth
    , size : Size
    }


type alias Properties msg =
    { body : ButtonBody
    , mode : ButtonMode msg
    }


type Button msg
    = Button (Properties msg) Options
    | Toggle (ToggleProperties msg) Options


type ButtonBody
    = BodyText String
    | BodyIcon Icon


type ButtonAction msg
    = ActionMsg msg
    | ActionRedirect Link


type ButtonMode msg
    = ButtonWorking (ButtonAction msg) ButtonStyle
    | ButtonDisabled
    | ButtonSuccess


type EmbossedTone
    = TonePrimary
    | ToneDanger
    | ToneLight
    | ToneClear


type ButtonStyle
    = StyleEmbossed EmbossedTone
    | StyleHyperlink


type ButtonWidth
    = WidthFull
    | WidthShrink


type alias ToggleProperties msg =
    { current : Bool
    , toggleMsg : Bool -> msg
    , hint : String
    }



-- Init


defaultOptions : Options
defaultOptions =
    { width = WidthShrink
    , size = Size.default
    }


toggle : String -> (Bool -> msg) -> Bool -> Button msg
toggle hint msg isEnabled =
    Toggle { toggleMsg = msg, current = isEnabled, hint = hint } defaultOptions


fromLabel : String -> ButtonBody
fromLabel label =
    BodyText label


fromIcon : Icon -> ButtonBody
fromIcon icon =
    BodyIcon icon



-- Body to Button


disabled : ButtonBody -> Button msg
disabled body =
    Button { mode = ButtonDisabled, body = body } defaultOptions


success : ButtonBody -> Button msg
success body =
    Button { mode = ButtonSuccess, body = body } defaultOptions


cmd : msg -> ButtonStyle -> ButtonBody -> Button msg
cmd msg style body =
    Button
        { mode = ButtonWorking (ActionMsg msg) style
        , body = body
        }
        defaultOptions


redirect : Link -> ButtonStyle -> ButtonBody -> Button msg
redirect link style body =
    Button
        { mode = ButtonWorking (ActionRedirect link) style
        , body = body
        }
        defaultOptions



-- Options


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


withWidth : ButtonWidth -> Button msg -> Button msg
withWidth width button =
    case button of
        Toggle prop opt ->
            Toggle prop { opt | width = width }

        Button prop opt ->
            Button prop { opt | width = width }


withSize : Size -> Button msg -> Button msg
withSize size button =
    case button of
        Toggle prop opt ->
            Toggle prop { opt | size = size }

        Button prop opt ->
            Button prop { opt | size = size }



-- Expose all properties


danger : ButtonStyle
danger =
    StyleEmbossed ToneDanger


light : ButtonStyle
light =
    StyleEmbossed ToneLight


clear : ButtonStyle
clear =
    StyleEmbossed ToneClear


primary : ButtonStyle
primary =
    StyleEmbossed TonePrimary


hyperlink : ButtonStyle
hyperlink =
    StyleHyperlink


full : ButtonWidth
full =
    WidthFull


shrink : ButtonWidth
shrink =
    WidthShrink



-- Etc


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
                ButtonWorking action style ->
                    Button { mode = ButtonWorking (newAction action) style, body = body } opt

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


renderElement : RenderConfig -> Button msg -> Element msg
renderElement cfg button =
    case button of
        Toggle { hint, current, toggleMsg } { size } ->
            toggleView cfg size hint toggleMsg current

        Button { mode, body } { size, width } ->
            case mode of
                ButtonWorking action StyleHyperlink ->
                    hyperlinkView cfg size width body action

                ButtonWorking action (StyleEmbossed tone) ->
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
        ( paddings, borders ) =
            iconLayout size

        attrs =
            [ ARIA.roleAttr ARIA.roleButton
            , Primitives.roundedBorders
            , paddings
            , borders
            , Events.onClick <| toggleMsg (not current)
            ]
                ++ toggleTheme current
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
            [ ARIA.roleAttr ARIA.roleButton
            , Primitives.roundedBorders
            , buttonWidth width
            , Palette.color tonePrimary brightnessMiddle
                |> Palette.toElColor
                |> Font.color
            , Font.regular
            , Font.underline
            , Element.pointer
            ]
    in
    case action of
        ActionRedirect link ->
            body
                |> bodyToElement cfg size
                |> Link.wrapElement cfg attrs link

        ActionMsg msg ->
            body
                |> bodyToElement cfg size
                |> Element.el (Events.onClick msg :: attrs)


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
        ( paddings, borders ) =
            bodyLayout body size

        attrs =
            [ ARIA.roleAttr ARIA.roleButton
            , Primitives.roundedBorders
            , buttonWidth width
            , paddings
            , borders
            , Font.semiBold
            , Element.pointer
            ]
                ++ workingTheme tone
    in
    case action of
        ActionRedirect link ->
            body
                |> bodyToElement cfg size
                |> Link.wrapElement cfg attrs link

        ActionMsg msg ->
            body
                |> bodyToElement cfg size
                |> Element.el (Events.onClick msg :: attrs)


staticView :
    RenderConfig
    -> Size
    -> ButtonWidth
    -> ButtonBody
    -> (ButtonBody -> List (Attribute msg))
    -> Element msg
staticView cfg size width body theme =
    let
        ( paddings, borders ) =
            bodyLayout body size

        attrs =
            [ Primitives.roundedBorders
            , buttonWidth width
            , paddings
            , borders
            , Font.semiBold
            ]
                ++ Element.disabled
                ++ theme body
    in
    body
        |> bodyToElement cfg size
        |> Element.el attrs


bodyToElement : RenderConfig -> Size -> ButtonBody -> Element msg
bodyToElement cfg size body =
    case body of
        BodyText str ->
            Element.el
                [ Font.size <| textSize size
                , Element.centerX
                , Element.spacing 8
                ]
                (Element.text str)

        BodyIcon icon ->
            icon
                |> Icon.withSize size
                |> Icon.renderElement cfg



-- Attributes


buttonWidth : ButtonWidth -> Attribute msg
buttonWidth width =
    case width of
        WidthFull ->
            Element.width Element.fill

        WidthShrink ->
            Element.width Element.shrink


bodyLayout : ButtonBody -> Size -> ( Attribute msg, Attribute msg )
bodyLayout body size =
    case body of
        BodyText _ ->
            textLayout size

        BodyIcon _ ->
            iconLayout size


iconLayout : Size -> ( Attribute msg, Attribute msg )
iconLayout size =
    let
        border =
            borderWidth size

        paddingXY =
            case size of
                Size.Large ->
                    ( (60 - 24) // 2 - border, (60 - 24) // 2 - border )

                Size.Medium ->
                    ( (48 - 20) // 2 - border, (48 - 20) // 2 - border )

                Size.Small ->
                    ( (36 - 16) // 2 - border, (36 - 16) // 2 - border )

                Size.ExtraSmall ->
                    ( (24 - 10) // 2 - border, (24 - 10) // 2 - border )
    in
    ( pairUncurry Element.paddingXY paddingXY
    , Border.width border
    )


textLayout : Size -> ( Attribute msg, Attribute msg )
textLayout size =
    let
        border =
            borderWidth size

        paddingXY =
            case size of
                Size.Large ->
                    ( (40 // 2) - border, ((60 - 20) // 2) - border )

                Size.Medium ->
                    ( (32 // 2) - border, ((48 - 16) // 2) - border )

                Size.Small ->
                    ( (20 // 2) - border, ((36 - 12) // 2) - border )

                Size.ExtraSmall ->
                    ( (12 // 2) - border, ((24 - 10) // 2) - border )
    in
    ( pairUncurry Element.paddingXY paddingXY
    , Border.width border
    )


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


textSize : Size -> Int
textSize size =
    case size of
        Size.Large ->
            20

        Size.Medium ->
            16

        Size.Small ->
            12

        Size.ExtraSmall ->
            10



-- Theme applier


type alias ThemeTriple =
    { background : Maybe Palette.Color
    , border : Maybe Palette.Color
    , text : TextColor
    }


type alias ButtonTheme =
    { normal : ThemeTriple
    , hover : Maybe ThemeTriple
    }


themeApply : ButtonTheme -> List (Attribute msg)
themeApply theme =
    case theme.hover of
        Just hoverTriple ->
            (hoverTriple
                |> themeToAttributes
                |> Element.mouseOver
            )
                :: Element.colorTransition 100
                ++ themeToAttributes theme.normal

        Nothing ->
            themeToAttributes theme.normal


themeToAttributes : ThemeTriple -> List (Element.Attr decorative msg)
themeToAttributes { background, border, text } =
    text
        |> Text.fontColor
        |> Maybe.map Font.color
        |> maybeToList
        |> prependMaybe (Maybe.map (Palette.toElColor >> Background.color) background)
        |> prependMaybe (Maybe.map (Palette.toElColor >> Border.color) border)



-- Themes


toggleTheme : Bool -> List (Attribute msg)
toggleTheme current =
    themeApply <|
        if current then
            primaryTheme

        else
            { normal =
                { background = Nothing
                , border = Just <| Palette.color Palette.tonePrimary brightnessMiddle
                , text =
                    Palette.color Palette.tonePrimary brightnessMiddle
                        |> Text.ColorPalette
                }
            , hover =
                Just
                    { background = Just <| Palette.color Palette.tonePrimary brightnessLightest
                    , border = Just <| Palette.color Palette.tonePrimary brightnessDarkest
                    , text =
                        Palette.color Palette.tonePrimary brightnessDarkest
                            |> Text.ColorPalette
                    }
            }


workingTheme : EmbossedTone -> List (Attribute msg)
workingTheme tone =
    themeApply <|
        case tone of
            TonePrimary ->
                primaryTheme

            ToneDanger ->
                { normal =
                    { background = Just <| Palette.color Palette.toneDanger brightnessMiddle
                    , border = Just <| Palette.color Palette.toneDanger brightnessMiddle
                    , text =
                        Palette.color Palette.toneDanger brightnessMiddle
                            |> Palette.setContrasting True
                            |> Text.ColorPalette
                    }
                , hover =
                    Just
                        { background = Just <| Palette.color Palette.toneDanger brightnessDarkest
                        , border = Just <| Palette.color Palette.toneDanger brightnessDarkest
                        , text =
                            Palette.color Palette.toneDanger brightnessDarkest
                                |> Palette.setContrasting True
                                |> Text.ColorPalette
                        }
                }

            ToneLight ->
                { normal =
                    { background = Just <| Palette.color Palette.toneGray brightnessLightest
                    , border = Just <| Palette.color Palette.toneGray brightnessLightest
                    , text =
                        Palette.color Palette.tonePrimary brightnessMiddle
                            |> Text.ColorPalette
                    }
                , hover =
                    Just
                        { background = Just <| Palette.color Palette.toneGray brightnessLighter
                        , border = Just <| Palette.color Palette.toneGray brightnessLighter
                        , text =
                            Palette.color Palette.tonePrimary brightnessDarkest
                                |> Text.ColorPalette
                        }
                }

            ToneClear ->
                { normal =
                    { background = Nothing
                    , border = Nothing
                    , text =
                        Palette.color Palette.tonePrimary brightnessMiddle
                            |> Text.ColorPalette
                    }
                , hover =
                    Just
                        { background = Just <| Palette.color Palette.toneGray brightnessLightest
                        , border = Just <| Palette.color Palette.toneGray brightnessLightest
                        , text =
                            Palette.color Palette.tonePrimary brightnessMiddle
                                |> Text.ColorPalette
                        }
                }


disabledTheme : ButtonBody -> List (Attribute msg)
disabledTheme body =
    themeApply <|
        case body of
            BodyIcon _ ->
                { normal =
                    { background = Just <| Palette.color Palette.toneGray brightnessLightest
                    , border = Just <| Palette.color Palette.toneGray brightnessLightest
                    , text =
                        Palette.color Palette.toneGray brightnessLight
                            |> Text.ColorPalette
                    }
                , hover = Nothing
                }

            BodyText _ ->
                { normal =
                    { background = Just <| Palette.color Palette.toneGray brightnessLight
                    , border = Just <| Palette.color Palette.toneGray brightnessLight
                    , text =
                        Palette.color Palette.toneGray brightnessLight
                            |> Palette.setContrasting True
                            |> Text.ColorPalette
                    }
                , hover = Nothing
                }


successTheme : ButtonBody -> List (Attribute msg)
successTheme _ =
    themeApply <|
        { normal =
            { background = Just <| Palette.color Palette.toneSuccess brightnessMiddle
            , border = Just <| Palette.color Palette.toneSuccess brightnessMiddle
            , text =
                Palette.color Palette.toneSuccess brightnessMiddle
                    |> Palette.setContrasting True
                    |> Text.ColorPalette
            }
        , hover =
            Just
                { background = Just <| Palette.color Palette.toneSuccess brightnessDarkest
                , border = Just <| Palette.color Palette.toneSuccess brightnessDarkest
                , text =
                    Palette.color Palette.toneSuccess brightnessDarkest
                        |> Palette.setContrasting True
                        |> Text.ColorPalette
                }
        }


primaryTheme : ButtonTheme
primaryTheme =
    { normal =
        { background = Just <| Palette.color Palette.tonePrimary brightnessMiddle
        , border = Just <| Palette.color Palette.tonePrimary brightnessMiddle
        , text =
            Palette.color Palette.tonePrimary brightnessMiddle
                |> Palette.setContrasting True
                |> Text.ColorPalette
        }
    , hover =
        Just
            { background = Just <| Palette.color Palette.tonePrimary brightnessDarkest
            , border = Just <| Palette.color Palette.tonePrimary brightnessDarkest
            , text =
                Palette.color Palette.tonePrimary brightnessDarkest
                    |> Palette.setContrasting True
                    |> Text.ColorPalette
            }
    }
