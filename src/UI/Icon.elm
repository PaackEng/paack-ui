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
    , todo
    , toggle
    , toggleDown
    , toggleUp
    , withColor
    , withSize
    )

import Element exposing (..)
import Element.Font as Font
import Html
import Html.Attributes as HtmlAttr
import UI.Internal.ContextualSize as ContextualSize exposing (ContextualSize)
import UI.Palette as Palette exposing (brightnessDarkest, toneGray)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Utils.ARIA as ARIA


todo : String -> Element msg
todo _ =
    -- TODO: Remove
    fasIcon "--------" ""


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
    | ToggleUp
    | BackwardContent
    | LeftArrow
    | RightArrow
    | SeeMore


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


toggleUp : String -> Icon
toggleUp hint =
    Icon (Properties hint ToggleUp) defaultOptions


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


toEl : RenderConfig -> Icon -> Element msg
toEl cfg (Icon { hint, glyph } { color, size }) =
    let
        staticAttrs =
            [ ARIA.roleAttr ARIA.roleImage
            , ARIA.labelAttr hint
            , Element.centerX
            , Font.center
            , Element.width <| Element.px width
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
                fasIcon "plus" hint

            Toggle ->
                fasIcon "map" hint

            ToggleDown ->
                fasIcon "chevron-down" hint

            ToggleUp ->
                fasIcon "chevron-up" hint

            Close ->
                fasIcon "times" hint

            SandwichMenu ->
                fasIcon "bars" hint

            Notifications ->
                fasIcon "bell" hint

            PaackSpaces ->
                fasIcon "database" hint

            Packages ->
                fasIcon "box-open" hint

            EventLog ->
                fasIcon "comment" hint

            Logout ->
                fasIcon "user-circle" hint

            Search ->
                fasIcon "search" hint

            Print ->
                fasIcon "print" hint

            Edit ->
                fasIcon "edit" hint

            BackwardContent ->
                fasIcon "chevron-left" hint

            LeftArrow ->
                fasIcon "chevron-left" hint

            RightArrow ->
                fasIcon "chevron-right" hint

            SeeMore ->
                fasIcon "ellipsis-h" hint


getHint : Icon -> String
getHint (Icon { hint } _) =
    hint



-- Internals


defaultOptions : Options
defaultOptions =
    { color = ColorInherit
    , size = ContextualSize.default
    }


fasIcon : String -> String -> Element msg
fasIcon icon hintText =
    faIcon "fas" icon hintText


farIcon : String -> String -> Element msg
farIcon icon hintText =
    faIcon "far" icon hintText


faIcon : String -> String -> String -> Element msg
faIcon prefix icon hintText =
    html
        (Html.i
            [ HtmlAttr.class (prefix ++ " fa-" ++ icon) ]
            []
        )
