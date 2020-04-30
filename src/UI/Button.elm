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
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import UI.Icons as Icon exposing (Icon)
import UI.Internal.Palette as Palette exposing (Color)
import UI.Internal.Primitives as Primitives
import UI.RenderConfig exposing (RenderConfig)
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


toEl : RenderConfig -> Button msg -> Element msg
toEl cfg ((Button { click, body } _) as btn) =
    let
        attrs =
            baseAttrs btn
                ++ styleAttrs btn
                ++ disabledAttrs btn
                ++ clickAttrs btn
    in
    case click of
        ClickHref url ->
            Element.link
                attrs
                { url = url
                , label = elFromBody cfg body
                }

        _ ->
            Element.el attrs <|
                elFromBody cfg body


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
            Element.paddingXY 32 12

        BodyIcon _ ->
            Element.paddingXY 10 12

        BodyEl _ ->
            Element.paddingXY 10 12


type alias ColorIntermediary =
    ( ( Color, Color ), Maybe ( Color, Color ) )


styleAttrs : Button msg -> List (Attribute msg)
styleAttrs ((Button { click } { style }) as btn) =
    let
        ( ( active, passive ) as normal, maybeHover ) =
            colorHelper btn

        ( hoverActive, hoverPassive ) =
            Maybe.withDefault normal maybeHover
    in
    case ( click, style ) of
        ( ClickHref _, _ ) ->
            [ Font.underline
            , Font.color passive
            , Element.mouseOver
                [ Font.color hoverPassive
                ]
            ]

        ( _, StyleFilled ) ->
            [ Background.color active
            , Font.color passive
            , Element.mouseOver
                [ Background.color hoverActive
                , Font.color hoverPassive
                ]
            ]
                ++ Element.colorTransition 100

        ( _, StyleOutlined ) ->
            [ Background.color passive
            , Font.color active
            , Element.mouseOver
                [ Background.color hoverPassive
                , Font.color hoverActive
                , Border.color hoverActive
                ]
            , Border.color active
            , Border.width 1
            ]
                ++ Element.colorTransition 100


colorHelper : Button msg -> ColorIntermediary
colorHelper (Button { click, body } { mode, tone }) =
    case click of
        ClickHref _ ->
            ( ( Palette.gray.lightest, Palette.primary.middle )
            , Just ( Palette.gray.lighter, Palette.primary.darkest )
            )

        _ ->
            if mode == ModeDisabled then
                case ( body, tone ) of
                    ( BodyIcon _, _ ) ->
                        ( ( Palette.gray.lightest, Palette.gray.light )
                        , Nothing
                        )

                    ( BodyText _, ToneLight ) ->
                        ( ( Palette.gray.lightest, Palette.textDisbledWithGrayLightest )
                        , Nothing
                        )

                    _ ->
                        ( ( Palette.gray.light, Palette.textWithBg )
                        , Nothing
                        )

            else
                case tone of
                    TonePrimary ->
                        ( ( Palette.primary.middle, Palette.textWithBg )
                        , Just ( Palette.primary.darkest, Palette.textWithBg )
                        )

                    ToneSuccess ->
                        ( ( Palette.success.middle, Palette.textWithBg )
                        , Just ( Palette.success.darkest, Palette.textWithBg )
                        )

                    ToneDanger ->
                        ( ( Palette.danger.middle, Palette.textWithBg )
                        , Just ( Palette.danger.darkest, Palette.textWithBg )
                        )

                    ToneLight ->
                        ( ( Palette.gray.lightest, Palette.textWithGrayLightest )
                        , Just ( Palette.gray.lighter, Palette.textWithGrayLighter )
                        )


isOutlined : Button msg -> Bool
isOutlined (Button { click } { style }) =
    case ( click, style ) of
        ( ClickToggle _ False, _ ) ->
            True

        ( _, StyleOutlined ) ->
            True

        _ ->
            False


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


elFromBody : RenderConfig -> ButtonBody msg -> Element msg
elFromBody cfg body =
    case body of
        BodyText str ->
            Element.text str

        BodyIcon ico ->
            Icon.toEl cfg ico

        BodyEl el ->
            el
