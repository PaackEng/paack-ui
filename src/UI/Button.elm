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
    , modeDisabled
    , modeEnabled
    , toEl
    , toggle
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
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Internal.Text as Text exposing (TextColor)
import UI.Link as Link exposing (Link)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element


type alias Options =
    { mode : ButtonMode
    , tone : ButtonTone
    , width : ButtonWidth
    }


type alias Properties msg =
    { body : ButtonBody msg
    , click : ButtonClick msg
    }


type Button msg
    = Button (Properties msg) Options


type ButtonBody msg
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


buttonAny : ButtonClick msg -> ButtonBody msg -> Button msg
buttonAny click body =
    Button (Properties body click) defaultOptions


button : msg -> ButtonBody msg -> Button msg
button msg =
    buttonAny (ClickMsg msg)


toggle : (Bool -> msg) -> Bool -> ButtonBody msg -> Button msg
toggle msg isEnabled =
    buttonAny (ClickToggle msg isEnabled)


link : Link -> ButtonBody msg -> Button msg
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


bodyIcon : Icon -> ButtonBody msg
bodyIcon icon =
    BodyIcon icon


bodyText : String -> ButtonBody msg
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



-- Render


toEl : RenderConfig -> Button msg -> Element msg
toEl cfg ((Button { click, body } _) as btn) =
    let
        attrs =
            baseAttrs btn
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
    Text.attributes cfg Text.SizeSubtitle1 Text.defaultColor


baseAttrs : Button msg -> List (Attribute msg)
baseAttrs btn =
    [ Primitives.roundedBorders
    , buttonWidth btn
    , buttonPadding btn
    ]


buttonWidth : Button msg -> Attribute msg
buttonWidth (Button _ { width }) =
    if width == WidthFull then
        Element.width Element.fill

    else
        Element.width Element.shrink


buttonPadding : Button msg -> Attribute msg
buttonPadding ((Button { body } _) as btn) =
    -- Remove 1 pixel each side for borders
    case body of
        BodyText _ ->
            Element.paddingXY 31 15

        BodyIcon _ ->
            Element.paddingXY 9 15


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


colorHelperWhenDisabled : ButtonBody msg -> ButtonTone -> ButtonTheme
colorHelperWhenDisabled body tone =
    case ( body, tone ) of
        ( BodyIcon _, _ ) ->
            { normal =
                { primary = ( Palette.toneGray, Palette.lumLightest )
                , text = Text.ColorBgDisabled
                , outlinedBg = ( Palette.toneGray, Palette.lumLight )
                }
            , hover = Nothing
            }

        ( BodyText _, ToneLight ) ->
            { normal =
                { primary = ( Palette.toneGray, Palette.lumLightest )
                , text = Text.ColorBgLightest
                , outlinedBg = ( Palette.toneGray, Palette.lumLight )
                }
            , hover = Nothing
            }

        _ ->
            { normal =
                { primary = ( Palette.toneGray, Palette.lumLight )
                , text = Text.ColorBgMiddle
                , outlinedBg = ( Palette.toneGray, Palette.lumLight )
                }
            , hover = Nothing
            }


colorHelperWhenEnabled : ButtonTone -> ButtonTheme
colorHelperWhenEnabled tone =
    case tone of
        TonePrimary ->
            { normal =
                { primary = ( Palette.tonePrimary, Palette.lumMiddle )
                , text = Text.ColorBgMiddle
                , outlinedBg = ( Palette.toneGray, Palette.lumLightest )
                }
            , hover =
                Just
                    { primary = ( Palette.tonePrimary, Palette.lumDarkest )
                    , text = Text.ColorBgMiddle
                    , outlinedBg = ( Palette.toneGray, Palette.lumLightest )
                    }
            }

        ToneSuccess ->
            { normal =
                { primary = ( Palette.toneSuccess, Palette.lumMiddle )
                , text = Text.ColorBgMiddle
                , outlinedBg = ( Palette.toneGray, Palette.lumLightest )
                }
            , hover =
                Just
                    { primary = ( Palette.toneSuccess, Palette.lumDarkest )
                    , text = Text.ColorBgMiddle
                    , outlinedBg = ( Palette.toneGray, Palette.lumLightest )
                    }
            }

        ToneDanger ->
            { normal =
                { primary = ( Palette.toneDanger, Palette.lumMiddle )
                , text = Text.ColorBgMiddle
                , outlinedBg = ( Palette.toneGray, Palette.lumLightest )
                }
            , hover =
                Just
                    { primary = ( Palette.toneDanger, Palette.lumDarkest )
                    , text = Text.ColorBgMiddle
                    , outlinedBg = ( Palette.toneGray, Palette.lumLightest )
                    }
            }

        ToneLight ->
            { normal =
                { primary = ( Palette.toneGray, Palette.lumLightest )
                , text = Text.ColorBgLightest
                , outlinedBg = ( Palette.toneGray, Palette.lumLightest )
                }
            , hover =
                Just
                    { primary = ( Palette.toneGray, Palette.lumLighter )
                    , text = Text.ColorBgLighter
                    , outlinedBg = ( Palette.toneGray, Palette.lumLightest )
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


elFromBody : RenderConfig -> ButtonBody msg -> Element msg
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
            Element.el [ Element.centerX ]
                (Icon.toEl cfg icon)
