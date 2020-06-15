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
    , rightArrow
    , sandwichMenu
    , search
    , seeMore
    , toEl
    , toggle
    , toggleDown
    , warning
    , withColor
    , withSize
    )

import Element exposing (..)
import Element.Font as Font
import Html
import Html.Attributes as HtmlAttr
import Svg
import Svg.Attributes as SvgAttrs
import UI.Internal.ContextualSize as ContextualSize exposing (ContextualSize)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Utils.ARIA as ARIA


type alias Properties =
    { hint : String
    , glyph : IconGlyph
    }


type alias Options =
    { color : IconColor
    , size : ContextualSize
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
    | ToggleDown
    | BackwardContent
    | LeftArrow
    | RightArrow
    | SeeMore
    | Warning


withColor : Palette.Color -> Icon -> Icon
withColor color (Icon prop opt) =
    Icon prop { opt | color = ColorFromPalette color }


withSize : ContextualSize -> Icon -> Icon
withSize size (Icon prop opt) =
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


toEl : RenderConfig -> Icon -> Element msg
toEl _ (Icon { hint, glyph } { color, size }) =
    let
        staticAttrs =
            [ ARIA.roleAttr ARIA.roleImage
            , ARIA.labelAttr hint
            , Element.centerX
            , Font.center
            , Element.width <| Element.px width
            , Element.height <| Element.px height
            , Font.size height
            ]

        attrs =
            case color of
                ColorFromPalette realColor ->
                    (realColor
                        |> Palette.toElColor
                        |> Font.color
                    )
                        :: staticAttrs

                ColorInherit ->
                    staticAttrs

        ( width, height ) =
            case size of
                ContextualSize.ExtraLarge ->
                    ( 26, 20 )

                ContextualSize.Large ->
                    ( 20, 16 )

                ContextualSize.Small ->
                    ( 16, 12 )
    in
    Element.el attrs <|
        case glyph of
            Add ->
                svgIcon "Add1"

            Toggle ->
                svgIcon "Map1"

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
    , size = ContextualSize.default
    }


svgIcon : String -> Element msg
svgIcon iconId =
    Element.html <|
        Svg.svg
            [ SvgAttrs.width "100%"
            , SvgAttrs.height "100%"
            , SvgAttrs.fill "currentColor"
            ]
            [ Svg.use [ SvgAttrs.xlinkHref ("#" ++ iconId) ] []
            ]


fasIcon : String -> String -> Element msg
fasIcon icon hintText =
    faIcon "fas" icon hintText


faIcon : String -> String -> String -> Element msg
faIcon prefix icon _ =
    html
        (Html.i
            [ HtmlAttr.class (prefix ++ " fa-" ++ icon) ]
            []
        )
