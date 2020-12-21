module UI.Internal.Button exposing (Button(..), ButtonAction(..), ButtonBody(..), ButtonMode(..), ButtonStyle(..), ButtonWidth(..), EmbossedTone(..), Options, Properties, ToggleProperties, bodyToElement)

import Element exposing (Element)
import Element.Font as Font
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Size as Size exposing (Size)
import UI.Link exposing (Link)
import UI.RenderConfig exposing (RenderConfig)
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
    = ButtonActive (ButtonAction msg) ButtonStyle
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
