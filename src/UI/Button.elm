module UI.Button exposing
    ( Button
    , ButtonClick
    , ButtonMode
    , ButtonStyle
    , ButtonTone
    , ButtonWidth
    , bodyEl
    , bodyIcon
    , bodyText
    , button
    , link
    , modeDisabled
    , modeEnabled
    , styleFilled
    , styleOutlined
    , toEl
    , toggle
    , toneDanger
    , toneLight
    , tonePrimary
    , toneSuccess
    , widthFull
    , widthRelative
    , withMode
    , withStyle
    , withTone
    , withWidth
    )

import Element exposing (Attribute, Element)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import UI.Icons exposing (Icon)
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Utils.Element as Element


type alias Options =
    { mode : ButtonMode
    , style : ButtonStyle
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
    | BodyEl (Element msg)


type ButtonClick msg
    = ClickMsg msg
    | ClickHref String
    | ClickToggle (Bool -> msg) Bool


type ButtonMode
    = ModeEnabled
    | ModeDisabled


type ButtonStyle
    = StyleFilled
    | StyleOutlined


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
    , style = StyleFilled
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


link : String -> ButtonBody msg -> Button msg
link url =
    buttonAny (ClickHref url)



-- Options


withMode : ButtonMode -> Button msg -> Button msg
withMode mode (Button prop opt) =
    Button prop { opt | mode = mode }


withStyle : ButtonStyle -> Button msg -> Button msg
withStyle style (Button prop opt) =
    Button prop { opt | style = style }


withTone : ButtonTone -> Button msg -> Button msg
withTone tone (Button prop opt) =
    Button prop { opt | tone = tone }


withWidth : ButtonWidth -> Button msg -> Button msg
withWidth width (Button prop opt) =
    Button prop { opt | width = width }



-- Expose all properties


bodyEl : Element msg -> ButtonBody msg
bodyEl elem =
    BodyEl elem


bodyIcon : Icon -> ButtonBody msg
bodyIcon ico =
    BodyIcon ico


bodyText : String -> ButtonBody msg
bodyText text =
    BodyText text


modeDisabled : ButtonMode
modeDisabled =
    ModeDisabled


modeEnabled : ButtonMode
modeEnabled =
    ModeEnabled


styleFilled : ButtonStyle
styleFilled =
    StyleFilled


styleOutlined : ButtonStyle
styleOutlined =
    StyleOutlined


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


toEl : Button msg -> Element msg
toEl ((Button { click, body } _) as btn) =
    let
        attrs =
            baseAttrs btn
                ++ colorAttrs btn
                ++ disabledAttrs btn
                ++ clickAttrs btn
    in
    case click of
        ClickHref url ->
            Element.link
                attrs
                { url = url
                , label = elFromBody body
                }

        _ ->
            Element.el attrs <|
                elFromBody body


baseAttrs : Button msg -> List (Attribute msg)
baseAttrs ((Button { click } _) as btn) =
    case click of
        ClickHref _ ->
            []

        _ ->
            [ Font.size 16
            , Font.center
            , Primitives.roundedBorders
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
buttonPadding (Button { body } _) =
    case body of
        BodyText _ ->
            Element.paddingXY 32 8

        BodyIcon _ ->
            Element.paddingXY 10 12

        BodyEl _ ->
            Element.paddingXY 10 12


colorAttrs : Button msg -> List (Attribute msg)
colorAttrs (Button _ { tone }) =
    []


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

        ( ModeEnabled, ClickHref _ ) ->
            []

        ( ModeDisabled, _ ) ->
            []


elFromBody : ButtonBody msg -> Element msg
elFromBody body =
    case body of
        BodyText str ->
            Element.text str

        BodyIcon _ ->
            Element.none

        BodyEl el ->
            el
