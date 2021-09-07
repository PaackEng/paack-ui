module UI.Icon exposing
    ( svgSpriteImport
    , Icon
    , add, assignPerson, boxes, check, close, collapse, configure, delete, done
    , download, edit, eventLog, expand, filter, fix, fixIssues, fixing, groups
    , loader, location, logout, moreActions, move, nextContent, notifications
    , paackSpaces, packages, person, persons, phone, pause, previousContent
    , print, reload, remove, sandwichMenu, search, searchSpace, seeMore
    , sortDecreasing, sortIncreasing, toggle, toggleDown, toggleUp, wait, warning
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

    Icon.logout "" Logout " from this account"
        |> Icon.renderElement renderConfig


# Embed

@docs svgSpriteImport


# Building

@docs Icon
@docs add, assignPerson, boxes, check, close, collapse, configure, delete, done
@docs download, edit, eventLog, expand, filter, fix, fixIssues, fixing, groups
@docs loader, location, logout, moreActions, move, nextContent, notifications
@docs paackSpaces, packages, person, persons, phone, pause, previousContent
@docs print, reload, remove, sandwichMenu, search, searchSpace, seeMore
@docs sortDecreasing, sortIncreasing, toggle, toggleDown, toggleUp, wait, warning


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
import Html.Attributes as HtmlAttrs
import Svg exposing (Svg)
import Svg.Attributes as SvgAttrs
import UI.Internal.Size as Size exposing (Size)
import UI.Internal.Svg as Svg
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Utils.ARIA as ARIA


type alias Properties =
    { hint : String
    , glyph : String
    }


type alias Options =
    { color : IconColor
    , size : Int
    , spin : Bool
    , notification : Bool
    }


type IconColor
    = ColorFromPalette Palette.Color
    | ColorInherit


{-| The `Icon` type is used for describing the component for later rendering.
-}
type Icon
    = Icon Properties Options



-- Options


{-| Icons colors can variate to match text color or contrast with a background.
See [`Palette.color`](UI-Palette#color) and [`Palette.setContrasting`](UI-Palette#setContrasting) for how to compose a valid color value.

    Icon.search "" Search " logs"
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
previousContent =
    defaultInit "Chevron-Left"


{-| An arrow pointing to the right used in chevrons and paginators.

    Icon.nextContent "Next page"

-}
nextContent : String -> Icon
nextContent =
    defaultInit "Chevron-Right"


{-| A foldable paper, toggle some content between showing/hiding, or full/collapsed.

    Icon.toggle "" Expand " technical details"

-}
toggle : String -> Icon
toggle =
    defaultInit "Map"


{-| An arrow pointing up.
May indicate the collapsing of the content below.

    Icon.toggleUp "" Collapse " details"

-}
toggleUp : String -> Icon
toggleUp =
    defaultInit "Chevron-Up"


{-| An arrow pointing down.
May indicate the expansion of a hidden content below.

    Icon.toggleDown "" Expand " details"

-}
toggleDown : String -> Icon
toggleDown =
    defaultInit "Chevron-Down"


{-| A plus sign. E.g., used to indicate the creation of new items in tables/lists.

    Icon.add "Create new user"

-}
add : String -> Icon
add =
    defaultInit "Add"


{-| Piled up boxes.

    Icon.packages "" Boxes - Filled ""

-}
boxes : String -> Icon
boxes =
    defaultInit "Boxes-Filled"


{-| A check mark, commonly used inside checkboxes and radio buttons.

    Icon.check "" Notepad ""

-}
check : String -> Icon
check =
    defaultInit "Checkmark"


{-| A times sign. Usually used for closing dialogs.

    Icon.close "" Close " this notification"

-}
close : String -> Icon
close =
    defaultInit "Close"


{-| A gear. Usually used for opening settings managers.

    Icon.configure "Open display settings"

-}
configure : String -> Icon
configure =
    defaultInit "Settings"


{-| Tree-bar stacked vertically.
It's usually used in mobile to toggle left bar menus.

    Icon.sandwichMenu "Open pages menu"

-}
sandwichMenu : String -> Icon
sandwichMenu =
    defaultInit "Hamburger"


{-| Three-dot in series horizontally.
It's usually used in the web to access less commonly used actions.

    Icon.moreActions "More actions"

-}
moreActions : String -> Icon
moreActions =
    defaultInit "Ellipsis"


{-| A bell for indicating notifications.

    Icon.notifications "See notifications"

-}
notifications : String -> Icon
notifications =
    defaultInit "Bell"


{-| Internal jargon in Paack.

    Icon.paackSpaces "Spaces"

-}
paackSpaces : String -> Icon
paackSpaces =
    defaultInit "Space"


{-| "Box-Outlined".

    Icon.packages "" Box - Outlined ""

-}
packages : String -> Icon
packages =
    defaultInit "Box-Outlined"


{-| A single person.

    Icon.person "Account"

-}
person : String -> Icon
person =
    defaultInit "Person"


{-| Three persons.

    Icon.persons "Contacts"

-}
persons : String -> Icon
persons =
    defaultInit "Persons"


{-| A phone.

    Icon.phone "Call"

-}
phone : String -> Icon
phone =
    defaultInit "Phone-StartCall"


{-| A chat-like baloon.
In Paack's apps this symbolizes the log of actions.

    Icon.eventLog "Events"

-}
eventLog : String -> Icon
eventLog =
    defaultInit "Message"


{-| An user/person profile's silhouette.

    Icon.logout "Leave/change account"

-}
logout : String -> Icon
logout =
    defaultInit "Logout"


{-| A magnifying glass. For use in search-related queries.

    Icon.search "" Search " package with barcode"

-}
search : String -> Icon
search =
    defaultInit "Search"


{-| An A4 ink printer.
Indicates the availability to print something related to the surrounding content.

    Icon.print "" Printer " pacakage's barcode"

-}
print : String -> Icon
print =
    defaultInit "Printer"


{-| A refresh sign (loop arrow).

    Icon.reload "Reload"

-}
reload : String -> Icon
reload =
    defaultInit "Reload"


{-| A pen. For editing fields/properties.

    Icon.edit "" Edit " contact informations"

-}
edit : String -> Icon
edit =
    defaultInit "Edit"


{-| Ellipsis (three-dots horizontally aligned).
For showing hidden details.

    Icon.seeMore "Read more about this article"

-}
seeMore : String -> Icon
seeMore =
    defaultInit "Ellipsis"


{-| A warning sign (a triangle with an exclamation mark).

    Icon.warning "" Warning ""

-}
warning : String -> Icon
warning =
    defaultInit "Warning"


{-| A funnel symbol represented by 3 lines horizontally aligned.

    Icon.filter "" Filter " title column"

-}
filter : String -> Icon
filter =
    defaultInit "Filter"


{-| A set of dots aligned as a six-sided polygon

    Icon.groups "" Groups ""

-}
groups : String -> Icon
groups =
    defaultInit "Groups"


{-| A group of squares acting as files with an arrow pointing to the bottom

    Icon.download "" Download " CSV"

-}
download : String -> Icon
download =
    defaultInit "Download"


{-| Two arrows from the center to the border

    Icon.expand "" Expand " view"

-}
expand : String -> Icon
expand =
    defaultInit "Expand"


{-| 2 Arrows from the border to the center

    Icon.collapse "" Collapse " view"

-}
collapse : String -> Icon
collapse =
    defaultInit "Collapse"


{-| The space icon with a search icon in the right-bottom.

    Icon.searchSpace "" Search " Space"

-}
searchSpace : String -> Icon
searchSpace =
    defaultInit "Space-Search"


{-| A circe with a line on center similar to a "No Way" road sign.

    Icon.remove "" Remove " item"

-}
remove : String -> Icon
remove =
    defaultInit "Remove"


{-| A trash can with an "x" on it.

    Icon.delete "" Trash " item"

-}
delete : String -> Icon
delete =
    defaultInit "Trash"


{-| Two vertical parallel arrows pointing to opposite directions.

    Icon.move "" Move " item"

-}
move : String -> Icon
move =
    defaultInit "Move"


{-| An arrow pointing down.

    Icon.sortIncreasing "Sort from A to Z"

-}
sortIncreasing : String -> Icon
sortIncreasing =
    defaultInit "Arrow-Down"


{-| An arrow pointing up.

    Icon.sortDecreasing "Sort from Z to A"

-}
sortDecreasing : String -> Icon
sortDecreasing =
    defaultInit "Arrow-Up"


{-| A monkey wrench.

    Icon.fix "" Fix " all invalid values"

-}
fix : String -> Icon
fix =
    defaultInit "Fix"


{-| A monkey wrench with a red ball.

    Icon.fixing "" Fix " something"

-}
fixing : String -> Icon
fixing hint =
    Icon
        (Properties hint "Fix")
        { defaultOptions | notification = True }


{-| A thunder bolt.

    Icon.fixIssues "" Fix " issues from selected groups"

-}
fixIssues : String -> Icon
fixIssues =
    defaultInit "Bolt"


{-| A person with a plus sign on the bottom-right.

    Icon.assingPerson "Select Manager"

-}
assignPerson : String -> Icon
assignPerson =
    defaultInit "Person-Assign"


{-| A notepad with a checkmark.

    Icon.done "Mark all as done."

-}
done : String -> Icon
done =
    defaultInit "Notepad"


{-| A GPS-like arrow.

    Icon.location "Track order."

-}
location : String -> Icon
location =
    defaultInit "Location"


{-| The hand from the stop sign.

    Icon.pause "Paused orders"

-}
pause : String -> Icon
pause =
    defaultInit "Hand"


{-| An hourglass.

    Icon.wait "On hold"

-}
wait : String -> Icon
wait =
    defaultInit "Hourglass"


{-| A loading spinner.

    Icon.spin "You spin me right 'round"

-}
loader : String -> Icon
loader hint =
    Icon
        (Properties hint "Loader")
        { defaultOptions | spin = True }


defaultInit : String -> String -> Icon
defaultInit glyph hint =
    Icon (Properties hint glyph) defaultOptions



-- Rendering


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Icon -> Element msg
renderElement _ (Icon { hint, glyph } opt) =
    let
        staticAttrs =
            (ARIA.toElementAttributes <| ARIA.roleImage hint)
                ++ [ Element.centerX
                   , Font.center
                   , Element.width <| Element.px opt.size
                   , Element.height <| Element.px opt.size
                   ]

        attrs =
            case opt.color of
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
        if opt.notification then
            notificationSvgIcon opt glyph

        else
            svgIcon opt glyph



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
    Html.div []
        [ Html.node "paack-svg-icon-sprite" [] []
        , circleMask
        ]



-- Internals


defaultOptions : Options
defaultOptions =
    { color = ColorInherit
    , notification = False
    , size = sizeToInt Size.default
    , spin = False
    }


svgIcon : Options -> String -> Element msg
svgIcon opt iconId =
    svgToHtml [ useIcon opt iconId ]


notificationSvgIcon : Options -> String -> Element msg
notificationSvgIcon opt iconId =
    svgToHtml
        [ Svg.g [ SvgAttrs.mask "url(#icon-circle-mask)" ]
            [ Svg.rect
                [ SvgAttrs.x "0"
                , SvgAttrs.y "0"
                , SvgAttrs.width "100%"
                , SvgAttrs.height "100%"
                , SvgAttrs.fill "transparent"
                ]
                []
            , useIcon opt iconId
            ]
        , Svg.circle
            [ SvgAttrs.fill <| Palette.toCssColor Palette.danger
            , SvgAttrs.r "17%"
            , SvgAttrs.cx "75%"
            , SvgAttrs.cy "20%"
            ]
            []
        ]


svgToHtml : List (Svg msg) -> Element msg
svgToHtml =
    Element.html
        << Svg.svg
            [ SvgAttrs.width "100%"
            , SvgAttrs.height "100%"
            , SvgAttrs.fill "currentColor"
            ]


useIcon : Options -> String -> Svg msg
useIcon { size, spin } iconId =
    Svg.use
        [ SvgAttrs.id iconId
        , SvgAttrs.xlinkHref ("#" ++ iconId)
        ]
    <|
        if spin then
            let
                center =
                    toFloat size / 2
            in
            [ Svg.animateSpin center center ]

        else
            []


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


circleMask : Svg msg
circleMask =
    Svg.svg
        [ SvgAttrs.version "1.1"
        , SvgAttrs.style "position:absolute"
        , SvgAttrs.width "0"
        , SvgAttrs.height "0"
        ]
        [ Svg.defs
            []
            [ Svg.mask
                [ SvgAttrs.id "icon-circle-mask"
                , SvgAttrs.maskUnits "objectBoundingBox"
                , SvgAttrs.maskContentUnits "objectBoundingBox"
                ]
                [ Svg.rect
                    [ SvgAttrs.fill "white"
                    , SvgAttrs.x "0"
                    , SvgAttrs.y "0"
                    , SvgAttrs.height "1"
                    , SvgAttrs.width "1"
                    ]
                    []
                , Svg.circle
                    [ SvgAttrs.fill "black"
                    , SvgAttrs.r "0.21"
                    , SvgAttrs.cx "0.75"
                    , SvgAttrs.cy "0.20"
                    ]
                    []
                ]
            ]
        ]
