module UI.Icon exposing
    ( Icon
    , add
    , backwardContent
    , close
    , edit
    , eventLog
    , getHint
    , leftArrow
    , logout
    , notifications
    , paackSpaces
    , packages
    , print
    , renderElement
    , rightArrow
    , sandwichMenu
    , search
    , seeMore
    , svgSpriteImport
    , toggle
    , toggleDown
    , toggleUp
    , warning
    , withColor
    , withCustomSize
    , withSize
    )

import Element exposing (..)
import Element.Font as Font
import Html
import Svg
import Svg.Attributes as SvgAttrs
import UI.Internal.Size as Size exposing (Size)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Utils.ARIA as ARIA


type alias Properties =
    { hint : String
    , glyph : IconGlyph
    }


type alias Options =
    { color : IconColor
    , size : Int
    }


type IconColor
    = ColorFromPalette Palette.Color
    | ColorInherit


type Icon
    = Icon Properties Options


type IconGlyph
    = Add
    | Close
    | Edit
    | EventLog
    | Logout
    | Notifications
    | PaackSpaces
    | Packages
    | Print
    | SandwichMenu
    | Search
    | Toggle
    | ToggleUp
    | ToggleDown
    | BackwardContent
    | LeftArrow
    | RightArrow
    | SeeMore
    | Warning


withColor : Palette.Color -> Icon -> Icon
withColor color (Icon prop opt) =
    Icon prop { opt | color = ColorFromPalette color }


withSize : Size -> Icon -> Icon
withSize size icon =
    withCustomSize (sizeToInt size) icon


withCustomSize : Int -> Icon -> Icon
withCustomSize size (Icon prop opt) =
    Icon prop { opt | size = size }


leftArrow : String -> Icon
leftArrow hint =
    Icon (Properties hint LeftArrow) defaultOptions


rightArrow : String -> Icon
rightArrow hint =
    Icon (Properties hint RightArrow) defaultOptions


toggle : String -> Icon
toggle hint =
    Icon (Properties hint Toggle) defaultOptions


toggleUp : String -> Icon
toggleUp hint =
    Icon (Properties hint ToggleUp) defaultOptions


toggleDown : String -> Icon
toggleDown hint =
    Icon (Properties hint ToggleDown) defaultOptions


add : String -> Icon
add hint =
    Icon (Properties hint Add) defaultOptions


close : String -> Icon
close hint =
    Icon (Properties hint Close) defaultOptions


sandwichMenu : String -> Icon
sandwichMenu hint =
    Icon (Properties hint SandwichMenu) defaultOptions


notifications : String -> Icon
notifications hint =
    Icon (Properties hint Notifications) defaultOptions


paackSpaces : String -> Icon
paackSpaces hint =
    Icon (Properties hint PaackSpaces) defaultOptions


packages : String -> Icon
packages hint =
    Icon (Properties hint Packages) defaultOptions


eventLog : String -> Icon
eventLog hint =
    Icon (Properties hint EventLog) defaultOptions


logout : String -> Icon
logout hint =
    Icon (Properties hint Logout) defaultOptions


search : String -> Icon
search hint =
    Icon (Properties hint Search) defaultOptions


print : String -> Icon
print hint =
    Icon (Properties hint Print) defaultOptions


edit : String -> Icon
edit hint =
    Icon (Properties hint Edit) defaultOptions


backwardContent : String -> Icon
backwardContent hint =
    Icon (Properties hint BackwardContent) defaultOptions


seeMore : String -> Icon
seeMore hint =
    Icon (Properties hint SeeMore) defaultOptions


warning : String -> Icon
warning hint =
    Icon (Properties hint Warning) defaultOptions


renderElement : RenderConfig -> Icon -> Element msg
renderElement _ (Icon { hint, glyph } { color, size }) =
    let
        staticAttrs =
            [ ARIA.roleAttr ARIA.roleImage
            , ARIA.labelAttr hint
            , Element.centerX
            , Font.center
            , Element.width <| Element.px size
            , Element.height <| Element.px size
            ]

        attrs =
            case color of
                ColorFromPalette realColor ->
                    (realColor
                        |> Palette.toElementColor
                        |> Font.color
                    )
                        :: staticAttrs

                ColorInherit ->
                    staticAttrs
    in
    Element.el attrs <|
        case glyph of
            Add ->
                svgIcon "Add1"

            Toggle ->
                svgIcon "Map1"

            ToggleUp ->
                svgIcon "UpArrow1"

            ToggleDown ->
                svgIcon "DownArrow1"

            Close ->
                svgIcon "Close1"

            SandwichMenu ->
                svgIcon "Hamburger"

            Notifications ->
                svgIcon "Bell"

            PaackSpaces ->
                svgIcon "Shelves1"

            Packages ->
                svgIcon "Map1"

            EventLog ->
                svgIcon "Messages1"

            Logout ->
                svgIcon "Person1"

            Search ->
                svgIcon "Search1"

            Print ->
                svgIcon "Print"

            Edit ->
                svgIcon "Edit"

            BackwardContent ->
                svgIcon "LeftArrow1"

            LeftArrow ->
                svgIcon "LeftArrow1"

            RightArrow ->
                svgIcon "RightArrow1"

            SeeMore ->
                svgIcon "More1"

            Warning ->
                svgIcon "Warning1"


getHint : Icon -> String
getHint (Icon { hint } _) =
    hint



-- Internals


defaultOptions : Options
defaultOptions =
    { color = ColorInherit
    , size = sizeToInt Size.default
    }


svgIcon : String -> Element msg
svgIcon iconId =
    Element.html <|
        Svg.svg
            [ SvgAttrs.width "100%"
            , SvgAttrs.height "100%"
            , SvgAttrs.fill "currentColor"
            ]
            [ Svg.use
                [ SvgAttrs.id iconId
                , SvgAttrs.xlinkHref ("#" ++ iconId)
                ]
                []
            ]


svgSpriteImport : Html.Html msg
svgSpriteImport =
    Html.node "paack-svg-icon-sprite"
        []
        []


sizeToInt : Size -> Int
sizeToInt size =
    case size of
        Size.Large ->
            24

        Size.Medium ->
            20

        Size.Small ->
            16

        Size.ExtraSmall ->
            10
