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
import UI.Internal.Palette as Palette exposing (Color)
import UI.Internal.Primitives as Primitives
import UI.Link as Link exposing (Link)
import UI.RenderConfig exposing (RenderConfig)
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


baseAttrs : Button msg -> List (Attribute msg)
baseAttrs btn =
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
buttonPadding ((Button { body } _) as btn) =
    -- Remove 1 pixel each side for borders
    case body of
        BodyText _ ->
            Element.paddingXY 31 11

        BodyIcon _ ->
            Element.paddingXY 9 11


ariaAttrs : List (Attribute msg)
ariaAttrs =
    [ ARIA.roleAttr ARIA.roleButton ]


type alias InvertableColor =
    -- This inverts when outlined
    { active : Color
    , passive : Color
    }


type alias ColorIntermediary =
    { normal : InvertableColor
    , onHover : Maybe InvertableColor
    }


styleAttrs : Button msg -> List (Attribute msg)
styleAttrs btn =
    let
        ({ normal } as interm) =
            btn
                |> colorHelper
                |> invertColorsWhen (isOutlined btn)

        hover =
            interm.onHover
                |> Maybe.withDefault normal
    in
    [ Background.color normal.active
    , Font.color normal.passive
    , Element.mouseOver
        [ Background.color hover.active
        , Font.color hover.passive
        , Border.color hover.passive
        ]
    , Border.color normal.passive
    , Border.width 1
    ]
        ++ Element.colorTransition 100


isOutlined : Button msg -> Bool
isOutlined (Button { click } _) =
    case click of
        ClickToggle _ False ->
            True

        _ ->
            False


invertColorsWhen : Bool -> ColorIntermediary -> ColorIntermediary
invertColorsWhen trigger ({ normal, onHover } as default) =
    let
        invert { active, passive } =
            { active = passive, passive = active }
    in
    if trigger then
        ColorIntermediary (invert normal)
            (Maybe.map invert onHover)

    else
        default


colorHelper : Button msg -> ColorIntermediary
colorHelper (Button { click, body } { mode, tone }) =
    if mode == ModeDisabled then
        case ( body, tone ) of
            ( BodyIcon _, _ ) ->
                ColorIntermediary
                    { active = Palette.gray.lightest, passive = Palette.gray.light }
                    Nothing

            ( BodyText _, ToneLight ) ->
                ColorIntermediary
                    { active = Palette.gray.lightest, passive = Palette.textDisbledWithGrayLightest }
                    Nothing

            _ ->
                ColorIntermediary
                    { active = Palette.gray.light, passive = Palette.textWithBg }
                    Nothing

    else
        case tone of
            TonePrimary ->
                ColorIntermediary
                    { active = Palette.primary.middle, passive = Palette.textWithBg }
                    (Just { active = Palette.primary.darkest, passive = Palette.textWithBg })

            ToneSuccess ->
                ColorIntermediary
                    { active = Palette.success.middle, passive = Palette.textWithBg }
                    (Just { active = Palette.success.darkest, passive = Palette.textWithBg })

            ToneDanger ->
                ColorIntermediary
                    { active = Palette.danger.middle, passive = Palette.textWithBg }
                    (Just { active = Palette.danger.darkest, passive = Palette.textWithBg })

            ToneLight ->
                ColorIntermediary
                    { active = Palette.gray.lightest, passive = Palette.textWithGrayLightest }
                    (Just { active = Palette.gray.lighter, passive = Palette.textWithGrayLighter })


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
            Element.text str

        BodyIcon icon ->
            Icon.toEl cfg icon
