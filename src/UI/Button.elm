module UI.Button exposing
    ( Button
    , ButtonClick
    , ButtonMode
    , ButtonTone
    , ButtonWidth
    , bodyIcon
    , bodyText
    , button
    , link
    , map
    , modeDisabled
    , modeEnabled
    , toEl
    , toggle
    , toneClear
    , toneDanger
    , toneLight
    , tonePrimary
    , toneSuccess
    , widthFull
    , widthRelative
    , withMode
    , withTone
    , withWidth
    )

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Basics exposing (lazyMap, pairUncurry)
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Internal.Text as Text exposing (TextColor)
import UI.Link as Link exposing (Link)
import UI.Palette as Palette exposing (brightnessDarkest, brightnessLight, brightnessLighter, brightnessLightest, brightnessMiddle)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element


type alias Options =
    { mode : ButtonMode
    , tone : ButtonTone
    , width : ButtonWidth
    }


type alias Properties msg =
    { body : ButtonBody
    , click : ButtonClick msg
    }


type Button msg
    = Button (Properties msg) Options


type ButtonBody
    = BodyText String
    | BodyIcon Icon


type ButtonClick msg
    = ClickMsg msg
    | ClickLink Link
    | ClickToggle (Bool -> msg) Bool


type ButtonMode
    = ModeEnabled
    | ModeDisabled


type ButtonTone
    = ToneDanger
    | ToneLight
    | TonePrimary
    | ToneSuccess
    | ToneClear


type ButtonWidth
    = WidthFull
    | WidthRelative



-- Create


defaultOptions : Options
defaultOptions =
    { mode = ModeEnabled
    , tone = TonePrimary
    , width = WidthRelative
    }


buttonAny : ButtonClick msg -> ButtonBody -> Button msg
buttonAny click body =
    Button (Properties body click) defaultOptions


button : msg -> ButtonBody -> Button msg
button msg =
    buttonAny (ClickMsg msg)


toggle : (Bool -> msg) -> Bool -> ButtonBody -> Button msg
toggle msg isEnabled =
    buttonAny (ClickToggle msg isEnabled)


link : Link -> ButtonBody -> Button msg
link realLink =
    buttonAny (ClickLink realLink)



-- Options


withMode : ButtonMode -> Button msg -> Button msg
withMode mode (Button prop opt) =
    Button prop { opt | mode = mode }


withTone : ButtonTone -> Button msg -> Button msg
withTone tone (Button prop opt) =
    Button prop { opt | tone = tone }


withWidth : ButtonWidth -> Button msg -> Button msg
withWidth width (Button prop opt) =
    Button prop { opt | width = width }



-- Expose all properties


bodyIcon : Icon -> ButtonBody
bodyIcon icon =
    BodyIcon icon


bodyText : String -> ButtonBody
bodyText text =
    BodyText text


modeDisabled : ButtonMode
modeDisabled =
    ModeDisabled


modeEnabled : ButtonMode
modeEnabled =
    ModeEnabled


toneDanger : ButtonTone
toneDanger =
    ToneDanger


toneLight : ButtonTone
toneLight =
    ToneLight


toneClear : ButtonTone
toneClear =
    ToneClear


tonePrimary : ButtonTone
tonePrimary =
    TonePrimary


toneSuccess : ButtonTone
toneSuccess =
    ToneSuccess


widthFull : ButtonWidth
widthFull =
    WidthFull


widthRelative : ButtonWidth
widthRelative =
    WidthRelative


map : (a -> b) -> Button a -> Button b
map applier (Button prop opt) =
    let
        newClick =
            case prop.click of
                ClickMsg msg ->
                    ClickMsg (applier msg)

                ClickLink realLink ->
                    ClickLink realLink

                ClickToggle lambda state ->
                    ClickToggle (lazyMap applier lambda) state
    in
    Button (Properties prop.body newClick) opt



-- Render


toEl : RenderConfig -> Button msg -> Element msg
toEl cfg ((Button { click, body } _) as btn) =
    let
        attrs =
            baseAttrs cfg btn
                ++ styleAttrs btn
                ++ disabledAttrs btn
                ++ clickAttrs btn
                ++ ariaAttrs
    in
    case click of
        ClickLink linkMeta ->
            body
                |> elFromBody cfg
                |> Link.packEl cfg attrs linkMeta

        _ ->
            Element.el attrs <|
                elFromBody cfg body


fontAttrs : RenderConfig -> List (Attribute msg)
fontAttrs cfg =
    Text.attributes cfg Text.SizeSubtitle1 True Text.defaultColor


baseAttrs : RenderConfig -> Button msg -> List (Attribute msg)
baseAttrs cfg btn =
    [ Primitives.roundedBorders
    , buttonWidth btn
    , buttonPadding cfg btn
    ]


buttonWidth : Button msg -> Attribute msg
buttonWidth (Button _ { width }) =
    if width == WidthFull then
        Element.width Element.fill

    else
        Element.width Element.shrink


buttonPadding : RenderConfig -> Button msg -> Attribute msg
buttonPadding cfg ((Button { body } _) as btn) =
    let
        -- Remove 1 pixel each side for borders
        paddingXY =
            case body of
                BodyText _ ->
                    ( 31, 15 )

                BodyIcon _ ->
                    case RenderConfig.getContextualSize cfg of
                        RenderConfig.SizeExtraLarge ->
                            ( (48 - 26) // 2 - 1, (48 - 20) // 2 - 1 )

                        RenderConfig.SizeLarge ->
                            ( (40 - 20) // 2 - 1, (40 - 16) // 2 - 1 )

                        RenderConfig.SizeSmall ->
                            ( (24 - 16) // 2 - 1, (24 - 12) // 2 - 1 )
    in
    pairUncurry Element.paddingXY paddingXY


ariaAttrs : List (Attribute msg)
ariaAttrs =
    [ ARIA.roleAttr ARIA.roleButton ]


type alias ThemeTriple =
    { primary : Palette.Color
    , text : TextColor
    , outlinedBg : Palette.Color
    }


type alias ButtonTheme =
    { normal : ThemeTriple
    , hover : Maybe ThemeTriple
    }


styleAttrs : Button msg -> List (Attribute msg)
styleAttrs btn =
    let
        theme =
            colorHelper btn

        hover ( bg, border, text ) =
            Element.mouseOver
                [ Background.color bg
                , Border.color border
                , Font.color text
                ]
                :: Element.colorTransition 100

        normal ( bg, border, text ) =
            [ Background.color bg
            , Border.color border
            , Border.width 1
            , Font.color text
            ]

        colors { primary, text, outlinedBg } =
            if isOutlined btn then
                ( Palette.toElColor outlinedBg
                , Palette.toElColor primary
                , Text.ColorPalette primary
                    |> Text.fontColor
                    |> Maybe.withDefault
                        Palette.gray.middle
                )

            else
                ( Palette.toElColor primary
                , Palette.toElColor primary
                , text
                    |> Text.fontColor
                    |> Maybe.withDefault
                        Palette.gray.middle
                )
    in
    case theme.hover of
        Just hoverTriple ->
            normal (colors theme.normal)
                ++ hover (colors hoverTriple)

        Nothing ->
            normal (colors theme.normal)


isOutlined : Button msg -> Bool
isOutlined (Button { click } _) =
    case click of
        ClickToggle _ False ->
            True

        _ ->
            False


colorHelper : Button msg -> ButtonTheme
colorHelper (Button { click, body } { mode, tone }) =
    if mode == ModeDisabled then
        colorHelperWhenDisabled body tone

    else
        colorHelperWhenEnabled tone


colorHelperWhenDisabled : ButtonBody -> ButtonTone -> ButtonTheme
colorHelperWhenDisabled body tone =
    case ( body, tone ) of
        ( BodyIcon _, _ ) ->
            { normal =
                { primary = Palette.color Palette.toneGray brightnessLightest
                , text =
                    Palette.color Palette.toneGray brightnessLight
                        |> Text.ColorPalette
                , outlinedBg = Palette.color Palette.toneGray brightnessLight
                }
            , hover = Nothing
            }

        ( BodyText _, ToneLight ) ->
            { normal =
                { primary = Palette.color Palette.toneGray brightnessLightest
                , text = Text.ColorForLightButtonDisabled
                , outlinedBg = Palette.color Palette.toneGray brightnessLight
                }
            , hover = Nothing
            }

        _ ->
            { normal =
                { primary = Palette.color Palette.toneGray brightnessLight
                , text =
                    Palette.color Palette.toneGray brightnessLight
                        |> Palette.setContrasting True
                        |> Text.ColorPalette
                , outlinedBg = Palette.color Palette.toneGray brightnessLight
                }
            , hover = Nothing
            }


colorHelperWhenEnabled : ButtonTone -> ButtonTheme
colorHelperWhenEnabled tone =
    case tone of
        TonePrimary ->
            { normal =
                { primary = Palette.color Palette.tonePrimary brightnessMiddle
                , text =
                    Palette.color Palette.tonePrimary brightnessMiddle
                        |> Palette.setContrasting True
                        |> Text.ColorPalette
                , outlinedBg = Palette.color Palette.toneGray brightnessLightest
                }
            , hover =
                Just
                    { primary = Palette.color Palette.tonePrimary brightnessDarkest
                    , text =
                        Palette.color Palette.tonePrimary brightnessDarkest
                            |> Palette.setContrasting True
                            |> Text.ColorPalette
                    , outlinedBg = Palette.color Palette.toneGray brightnessLightest
                    }
            }

        ToneSuccess ->
            { normal =
                { primary = Palette.color Palette.toneSuccess brightnessMiddle
                , text =
                    Palette.color Palette.toneSuccess brightnessMiddle
                        |> Palette.setContrasting True
                        |> Text.ColorPalette
                , outlinedBg = Palette.color Palette.toneGray brightnessLightest
                }
            , hover =
                Just
                    { primary = Palette.color Palette.toneSuccess brightnessDarkest
                    , text =
                        Palette.color Palette.toneSuccess brightnessDarkest
                            |> Palette.setContrasting True
                            |> Text.ColorPalette
                    , outlinedBg = Palette.color Palette.toneGray brightnessLightest
                    }
            }

        ToneDanger ->
            { normal =
                { primary = Palette.color Palette.toneDanger brightnessMiddle
                , text =
                    Palette.color Palette.toneDanger brightnessMiddle
                        |> Palette.setContrasting True
                        |> Text.ColorPalette
                , outlinedBg = Palette.color Palette.toneGray brightnessLightest
                }
            , hover =
                Just
                    { primary = Palette.color Palette.toneDanger brightnessDarkest
                    , text =
                        Palette.color Palette.toneDanger brightnessDarkest
                            |> Palette.setContrasting True
                            |> Text.ColorPalette
                    , outlinedBg = Palette.color Palette.toneGray brightnessLightest
                    }
            }

        ToneLight ->
            { normal =
                { primary = Palette.color Palette.toneGray brightnessLightest
                , text =
                    Palette.color Palette.tonePrimary brightnessMiddle
                        |> Text.ColorPalette
                , outlinedBg = Palette.color Palette.toneGray brightnessLightest
                }
            , hover =
                Just
                    { primary = Palette.color Palette.toneGray brightnessLighter
                    , text =
                        Palette.color Palette.tonePrimary brightnessDarkest
                            |> Text.ColorPalette
                    , outlinedBg = Palette.color Palette.toneGray brightnessLightest
                    }
            }

        ToneClear ->
            { normal =
                { primary =
                    Palette.color Palette.toneGray brightnessLightest
                        |> Palette.withAlpha 0
                , text =
                    Palette.color Palette.tonePrimary brightnessMiddle
                        |> Text.ColorPalette
                , outlinedBg = Palette.color Palette.toneGray brightnessLightest
                }
            , hover =
                Just
                    { primary =
                        Palette.color Palette.toneGray brightnessLightest
                    , text =
                        Palette.color Palette.tonePrimary brightnessMiddle
                            |> Text.ColorPalette
                    , outlinedBg = Palette.color Palette.toneGray brightnessLightest
                    }
            }


disabledAttrs : Button msg -> List (Attribute msg)
disabledAttrs (Button _ { mode }) =
    if mode == ModeDisabled then
        Element.disabled

    else
        [ Element.pointer ]


clickAttrs : Button msg -> List (Attribute msg)
clickAttrs (Button { click } { mode }) =
    case ( mode, click ) of
        ( ModeEnabled, ClickMsg msg ) ->
            [ Events.onClick msg ]

        ( ModeEnabled, ClickToggle msg isEnabled ) ->
            [ Events.onClick (msg (not isEnabled)) ]

        ( ModeEnabled, ClickLink _ ) ->
            []

        ( ModeDisabled, _ ) ->
            []


elFromBody : RenderConfig -> ButtonBody -> Element msg
elFromBody cfg body =
    case body of
        BodyText str ->
            Element.el
                [ Font.size 16
                , Element.spacing 8
                , Font.semiBold
                , Element.centerX
                ]
                (Element.text str)

        BodyIcon icon ->
            Icon.toEl cfg icon
