module UI.Icon exposing
    ( svgSpriteImport
    , Icon, add, close, edit, eventLog, previousContent, logout, notifications, paackSpaces, packages, print, nextContent, sandwichMenu, search, seeMore, toggle, toggleDown, toggleUp, warning
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

@docs Icon, add, close, edit, eventLog, previousContent, logout, notifications, paackSpaces, packages, print, nextContent, sandwichMenu, search, seeMore, toggle, toggleDown, toggleUp, warning


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
    | UpArrow
    | DownArrow
    | BackwardContent
    | LeftArrow
    | RightArrow
    | SeeMore
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
    Icon (Properties hint LeftArrow) defaultOptions


{-| An arrow pointing to the right used in chevrons and paginators.

    Icon.nextContent "Next page"

-}
nextContent : String -> Icon
nextContent hint =
    Icon (Properties hint RightArrow) defaultOptions


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
    Icon (Properties hint UpArrow) defaultOptions


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


{-| A times sign. Usually used for closing dialogs.

    Icon.close "Close this notification"

-}
close : String -> Icon
close hint =
    Icon (Properties hint Close) defaultOptions


{-| Tree-bar stacked vertically.
It's usually used in mobile to toggle left bar menus.

    Icon.sandwichMenu "Open pages menu"

-}
sandwichMenu : String -> Icon
sandwichMenu hint =
    Icon (Properties hint SandwichMenu) defaultOptions


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
                svgIcon "Add1"

            Toggle ->
                svgIcon "Map1"

            UpArrow ->
                svgIcon "UpArrow1"

            DownArrow ->
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
            24

        Size.Medium ->
            20

        Size.Small ->
            16

        Size.ExtraSmall ->
            10
