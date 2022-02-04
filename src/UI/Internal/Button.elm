module UI.Internal.Button exposing
    ( Button(..)
    , ButtonAction(..)
    , ButtonBody(..)
    , ButtonMode(..)
    , ButtonStyle(..)
    , ButtonWidth(..)
    , EmbossedTone(..)
    , Options
    , Properties
    , ToggleProperties
    , bodyToElement
    )

import Element exposing (Element)
import Element.Font as Font
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Size as Size exposing (Size)
import UI.Link exposing (Link)
import UI.RenderConfig exposing (RenderConfig)


type alias Options =
    { width : ButtonWidth
    , size : Size
    , id : Maybe String
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
    | BodyIconLeftText Icon
    | BodyIconRightText Icon


type ButtonAction msg
    = ActionMsg msg
    | ActionRedirect Link


type ButtonMode msg
    = ButtonActive (ButtonAction msg) ButtonStyle
    | ButtonDisabled


type EmbossedTone
    = TonePrimary
    | ToneDanger
    | ToneLight
    | ToneClear
    | ToneSwitchedOn


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


bodyToElement : RenderConfig -> Size -> ButtonBody -> Element msg
bodyToElement cfg size body =
    let
        attrs =
            [ Font.size <| textSize size
            , Element.centerX
            ]

        label str =
            Element.text str
    in
    case body of
        BodyText str ->
            Element.el attrs (label str)

        BodyIcon icon ->
            icon
                |> Icon.withSize size
                |> Icon.renderElement cfg

        BodyIconLeftText icon ->
            Element.row
                (Element.spacing (companionSpacing size) :: attrs)
                [ label <| Icon.getHint icon
                , companionIcon cfg size icon
                ]

        BodyIconRightText icon ->
            Element.row
                (Element.spacing (companionSpacing size) :: attrs)
                [ companionIcon cfg size icon
                , label <| Icon.getHint icon
                ]


companionIcon : RenderConfig -> Size -> Icon -> Element msg
companionIcon cfg size icon =
    icon
        |> Icon.withCustomSize (companionIconSize size)
        |> Icon.renderElement cfg


companionIconSize : Size -> Int
companionIconSize size =
    case size of
        Size.Large ->
            28

        Size.Medium ->
            20

        Size.Small ->
            16

        Size.ExtraSmall ->
            12


companionSpacing : Size -> Int
companionSpacing size =
    case size of
        Size.Large ->
            8

        Size.Medium ->
            4

        Size.Small ->
            4

        Size.ExtraSmall ->
            2


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
