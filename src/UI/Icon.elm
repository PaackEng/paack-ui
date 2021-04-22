module UI.Icon exposing
    ( svgSpriteImport
    , Icon
    , add, check, close, collapse, configure, download, edit, fix, fixIssues, eventLog
    , expand, filter, groups, logout, move, nextContent, notifications
    , paackSpaces, packages, previousContent, print, remove, sandwichMenu
    , search, searchSpace, seeMore, sortDecreasing, sortIncreasing, toggle
    , toggleDown, toggleUp, delete, warning, moreActions
    , getHint
    , withColor
    , withSize, withCustomSize
    , renderElement
    )

{-| `UI.Icon` is an implementation of icons using an SVG-spritesheet.

Icons' names are after their proposed action/symbolism instead of describing its shape.
Consequently, we don't use the same icon for different operations as these would confuse the end-users.

To use these icons, first, you must insert the spritesheet once (and only once) in the layout.
The sprite sheet injection uses a custom `Html` component that later populates using a parcel's import.
See [`Icon.svgSpriteImport`](UI-Icon#svgSpriteImport) to know what to do on the Elm's side.
Parcel's instructions are in [README](https://github.com/PaackEng/paack-ui/blob/master/README.md).

An icon can be created and rendered as in the following pipeline:

    Icon.logout "Logout from this account"
        |> Icon.renderElement renderConfig


# Embed

@docs svgSpriteImport


# Building

@docs Icon
@docs add, check, close, collapse, configure, download, edit, fix, fixIssues, eventLog
@docs expand, filter, groups, logout, move, nextContent, notifications
@docs paackSpaces, packages, previousContent, print, remove, sandwichMenu
@docs search, searchSpace, seeMore, sortDecreasing, sortIncreasing, toggle
@docs toggleDown, toggleUp, delete, warning, moreActions


# Disassemble

@docs getHint


# Color

@docs withColor


# Size

@docs withSize, withCustomSize


# Rendering

@docs renderElement

-}

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


{-| The `Icon` type is used for describing the component for later rendering.
-}
type Icon
    = Icon Properties Options


{-| When addig new icons, name them as "actions" and not what they look like.
For more information read this module's main documentation.
-}
type IconGlyph
    = Add
    | Check
    | Close
    | Collapse
    | Configure
    | Delete
    | DownArrow
    | Download
    | Edit
    | EventLog
    | Expand
    | Filter
    | Fix
    | FixIssues
    | Groups
    | Logout
    | Move
    | NextContent
    | Notifications
    | PaackSpaces
    | Packages
    | PreviousContent
    | Print
    | Remove
    | SandwichMenu
    | MoreActions
    | Search
    | SearchSpace
    | SeeMore
    | SortDecreasing
    | SortIncreasing
    | Toggle
    | ToggleUp
    | Warning



-- Options


{-| Icons colors can variate to match text color or contrast with a background.
See [`Palette.color`](UI-Palette#color) and [`Palette.setContrasting`](UI-Palette#setContrasting) for how to compose a valid color value.

    Icon.search "Search logs"
        |> Icon.withColor (Palette.color Palette.tonePrimary Palette.brightnessMiddle)
        |> Icon.renderElement renderConfig

-}
withColor : Palette.Color -> Icon -> Icon
withColor color (Icon prop opt) =
    Icon prop { opt | color = ColorFromPalette color }


{-| With `Icon.withSize`, you'll be able to scale the icon between the [standard sizes][size].

[size]: UI-Size

    Icon.withSize Size.large someIcon

**NOTE**: Default value is [`Size.medium`](UI-Size#medium).

-}
withSize : Size -> Icon -> Icon
withSize size icon =
    withCustomSize (sizeToInt size) icon


{-| With `Icon.withCustomSize`, you'll be able to scale the icon using an integer value.

All Icons are constraint to fit inside a 1:1 square.
So the set value also coincides with this square's side length.
E.g., setting `Icon.withCustomSize 48` will produce a square with 48px on each side.

    Icon.withCustomSize 48 someIcon

**NOTE**: Default value is 20.

-}
withCustomSize : Int -> Icon -> Icon
withCustomSize size (Icon prop opt) =
    Icon prop { opt | size = size }



-- Icons


{-| An arrow pointing to the left used in chevrons and paginators.

    Icon.previousContent "Previous page"

-}
previousContent : String -> Icon
previousContent hint =
    Icon (Properties hint PreviousContent) defaultOptions


{-| An arrow pointing to the right used in chevrons and paginators.

    Icon.nextContent "Next page"

-}
nextContent : String -> Icon
nextContent hint =
    Icon (Properties hint NextContent) defaultOptions


{-| A foldable paper, toggle some content between showing/hiding, or full/collapsed.

    Icon.toggle "Expand technical details"

-}
toggle : String -> Icon
toggle hint =
    Icon (Properties hint Toggle) defaultOptions


{-| An arrow pointing up.
May indicate the collapsing of the content below.

    Icon.toggleUp "Collapse details"

-}
toggleUp : String -> Icon
toggleUp hint =
    Icon (Properties hint ToggleUp) defaultOptions


{-| An arrow pointing down.
May indicate the expansion of a hidden content below.

    Icon.toggleDown "Expand details"

-}
toggleDown : String -> Icon
toggleDown hint =
    Icon (Properties hint DownArrow) defaultOptions


{-| A plus sign. E.g., used to indicate the creation of new items in tables/lists.

    Icon.add "Create new user"

-}
add : String -> Icon
add hint =
    Icon (Properties hint Add) defaultOptions


{-| A check mark, commonly used inside checkboxes and radio buttons.

    Icon.check "Done"

-}
check : String -> Icon
check hint =
    Icon (Properties hint Check) defaultOptions


{-| A times sign. Usually used for closing dialogs.

    Icon.close "Close this notification"

-}
close : String -> Icon
close hint =
    Icon (Properties hint Close) defaultOptions


{-| A gear. Usually used for opening settings managers.

    Icon.configure "Open display settings"

-}
configure : String -> Icon
configure hint =
    Icon (Properties hint Configure) defaultOptions


{-| Tree-bar stacked vertically.
It's usually used in mobile to toggle left bar menus.

    Icon.sandwichMenu "Open pages menu"

-}
sandwichMenu : String -> Icon
sandwichMenu hint =
    Icon (Properties hint SandwichMenu) defaultOptions


{-| Three-dot in series horizontally.
It's usually used in the web to access less commonly used actions.

    Icon.moreActions "More actions"

-}
moreActions : String -> Icon
moreActions hint =
    Icon (Properties hint MoreActions) defaultOptions


{-| A bell for indicating notifications.

    Icon.notifications "See notifications"

-}
notifications : String -> Icon
notifications hint =
    Icon (Properties hint Notifications) defaultOptions


{-| Internal jargon in Paack.

    Icon.paackSpaces "Spaces"

-}
paackSpaces : String -> Icon
paackSpaces hint =
    Icon (Properties hint PaackSpaces) defaultOptions


{-| Packages.

    Icon.packages "Packages"

-}
packages : String -> Icon
packages hint =
    Icon (Properties hint Packages) defaultOptions


{-| A chat-like baloon.
In Paack's apps this symbolizes the log of actions.

    Icon.eventLog "Events"

-}
eventLog : String -> Icon
eventLog hint =
    Icon (Properties hint EventLog) defaultOptions


{-| An user/person profile's silhouette.

    Icon.logout "Leave/change account"

-}
logout : String -> Icon
logout hint =
    Icon (Properties hint Logout) defaultOptions


{-| A magnifying glass. For use in search-related queries.

    Icon.search "Search package with barcode"

-}
search : String -> Icon
search hint =
    Icon (Properties hint Search) defaultOptions


{-| An A4 ink printer.
Indicates the availability to print something related to the surrounding content.

    Icon.print "Print pacakage's barcode"

-}
print : String -> Icon
print hint =
    Icon (Properties hint Print) defaultOptions


{-| A pen. For editing fields/properties.

    Icon.edit "Edit contact informations"

-}
edit : String -> Icon
edit hint =
    Icon (Properties hint Edit) defaultOptions


{-| Ellipsis (three-dots horizontally aligned).
For showing hidden details.

    Icon.seeMore "Read more about this article"

-}
seeMore : String -> Icon
seeMore hint =
    Icon (Properties hint SeeMore) defaultOptions


{-| A warning sign (a triangle with an exclamation mark).

    Icon.warning "Warning"

-}
warning : String -> Icon
warning hint =
    Icon (Properties hint Warning) defaultOptions


{-| A funnel symbol represented by 3 lines horizontally aligned.

    Icon.filter "Filter title column"

-}
filter : String -> Icon
filter hint =
    Icon (Properties hint Filter) defaultOptions


{-| A set of dots aligned as a six-sided polygon

    Icon.groups "Groups"

-}
groups : String -> Icon
groups hint =
    Icon (Properties hint Groups) defaultOptions


{-| A group of squares acting as files with an arrow pointing to the bottom

    Icon.download "Download CSV"

-}
download : String -> Icon
download hint =
    Icon (Properties hint Download) defaultOptions


{-| Two arrows from the center to the border

    Icon.expand "Expand view"

-}
expand : String -> Icon
expand hint =
    Icon (Properties hint Expand) defaultOptions


{-| 2 Arrows from the border to the center

    Icon.collapse "Collapse view"

-}
collapse : String -> Icon
collapse hint =
    Icon (Properties hint Collapse) defaultOptions


{-| The space icon with a search icon in the right-bottom.

    Icon.searchSpace "Search Space"

-}
searchSpace : String -> Icon
searchSpace hint =
    Icon (Properties hint SearchSpace) defaultOptions


{-| A circe with a line on center similar to a "No Way" road sign.

    Icon.remove "Remove item"

-}
remove : String -> Icon
remove hint =
    Icon (Properties hint Remove) defaultOptions


{-| A trash can with an "x" on it.

    Icon.delete "Delete item"

-}
delete : String -> Icon
delete hint =
    Icon (Properties hint Delete) defaultOptions


{-| Two vertical parallel arrows pointing to opposite directions.

    Icon.move "Move item"

-}
move : String -> Icon
move hint =
    Icon (Properties hint Move) defaultOptions


{-| An arrow pointing down.

    Icon.sortIncreasing "Sort from A to Z"

-}
sortIncreasing : String -> Icon
sortIncreasing hint =
    Icon (Properties hint SortIncreasing) defaultOptions


{-| An arrow pointing up.

    Icon.sortDecreasing "Sort from Z to A"

-}
sortDecreasing : String -> Icon
sortDecreasing hint =
    Icon (Properties hint SortDecreasing) defaultOptions


{-| A monkey wrench.

    Icon.fix "Fix all invalid values"

-}
fix : String -> Icon
fix hint =
    Icon (Properties hint Fix) defaultOptions


{-| A thunder bolt.

    Icon.fixIssues "Fix issues from selected groups"

-}
fixIssues : String -> Icon
fixIssues hint =
    Icon (Properties hint FixIssues) defaultOptions



-- Rendering


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Icon -> Element msg
renderElement _ (Icon { hint, glyph } { color, size }) =
    let
        staticAttrs =
            (ARIA.toElementAttributes <| ARIA.roleImage hint)
                ++ [ Element.centerX
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
                svgIcon "Add"

            Check ->
                svgIcon "Checkmark"

            Close ->
                svgIcon "Close"

            Collapse ->
                svgIcon "Collapse"

            Configure ->
                svgIcon "Settings"

            Delete ->
                svgIcon "Trash"

            DownArrow ->
                svgIcon "Chevron-Down"

            Download ->
                svgIcon "Download"

            Edit ->
                svgIcon "Edit"

            EventLog ->
                svgIcon "Message"

            Expand ->
                svgIcon "Expand"

            Filter ->
                svgIcon "Filter"

            Fix ->
                svgIcon "Fix"

            FixIssues ->
                svgIcon "Bolt"

            Groups ->
                svgIcon "Groups"

            Logout ->
                svgIcon "Logout"

            Move ->
                svgIcon "Move"

            NextContent ->
                svgIcon "Chevron-Right"

            Notifications ->
                svgIcon "Bell"

            PaackSpaces ->
                svgIcon "Space"

            Packages ->
                svgIcon "Box-Outlined"

            PreviousContent ->
                svgIcon "Chevron-Left"

            Print ->
                svgIcon "Printer"

            Remove ->
                svgIcon "Remove"

            SandwichMenu ->
                svgIcon "Hamburger"

            MoreActions ->
                svgIcon "Ellipsis"

            Search ->
                svgIcon "Search"

            SearchSpace ->
                svgIcon "Space-Search"

            SeeMore ->
                svgIcon "Ellipsis"

            SortDecreasing ->
                svgIcon "Arrow-Up"

            SortIncreasing ->
                svgIcon "Arrow-Down"

            Toggle ->
                svgIcon "Map"

            ToggleUp ->
                svgIcon "Chevron-Up"

            Warning ->
                svgIcon "Warning"



-- Disassemble


{-| For not creating data duplication, `Icon.getHint` can extract the hint from an `Icon`.

    let
        icon =
            Icon.seeMore "Read more about this"
    in
    Element.row []
        [ Icon.renderElement renderConfig icon
        , Icon.getHint icon
            |> Text.body1
            |> Text.renderElement renderConfig
        ]

-}
getHint : Icon -> String
getHint (Icon { hint } _) =
    hint



-- Layout


{-| Imports the SVG-spritesheet with all icons into the rendered HTML.

There is no need for using this function when you're using [`UI.NavigationContainer`][nav], and you should be using it.
But, in case you aren't, you need to insert this function on the most top component, which probably is the [`Element.layout`][layout], like this:

[nav]: UI-NavigationContainer
[layout]: /packages/mdgriffith/elm-ui/latest/Element#layout

    main : Program Flags Model Msg
    main =
        { yourProgram
            | view =
                { title = yourPageTitle
                , body =
                    [ Icon.svgSpriteImport
                    , Element.layout someAttributes yourPageView
                    ]
                }
        }

**NOTE**: Use [`UI.NavigationContainer`][nav]!

-}
svgSpriteImport : Html.Html msg
svgSpriteImport =
    Html.node "paack-svg-icon-sprite"
        []
        []



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


sizeToInt : Size -> Int
sizeToInt size =
    case size of
        Size.Large ->
            40

        Size.Medium ->
            32

        Size.Small ->
            24

        Size.ExtraSmall ->
            16
