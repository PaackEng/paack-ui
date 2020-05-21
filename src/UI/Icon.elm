module UI.Icon exposing
    ( Icon
    , add
    , close
    , edit
    , eventLog
    , getHint
    , logout
    , notifications
    , paackSpaces
    , packages
    , print
    , sandwichMenu
    , search
    , toEl
    , todo
    , toggle
    )

import Element exposing (..)
import Html
import Html.Attributes as HtmlAttr
import UI.RenderConfig exposing (RenderConfig)


todo : String -> Element msg
todo _ =
    -- TODO: Remove
    fasIcon "--------" ""


type alias Properties =
    { hint : String
    , glyph : IconGlyph
    }


type Icon
    = Icon Properties


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


toggle : String -> Icon
toggle hint =
    Icon (Properties hint Toggle)


add : String -> Icon
add hint =
    Icon (Properties hint Add)


close : String -> Icon
close hint =
    Icon (Properties hint Close)


sandwichMenu : String -> Icon
sandwichMenu hint =
    Icon (Properties hint SandwichMenu)


notifications : String -> Icon
notifications hint =
    Icon (Properties hint Notifications)


paackSpaces : String -> Icon
paackSpaces hint =
    Icon (Properties hint PaackSpaces)


packages : String -> Icon
packages hint =
    Icon (Properties hint Packages)


eventLog : String -> Icon
eventLog hint =
    Icon (Properties hint EventLog)


logout : String -> Icon
logout hint =
    Icon (Properties hint Logout)


search : String -> Icon
search hint =
    Icon (Properties hint Search)


print : String -> Icon
print hint =
    Icon (Properties hint Print)


edit : String -> Icon
edit hint =
    Icon (Properties hint Edit)


toEl : RenderConfig -> Icon -> Element msg
toEl _ (Icon { hint, glyph }) =
    case glyph of
        Add ->
            fasIcon "plus" hint

        Toggle ->
            fasIcon "map" hint

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


getHint : Icon -> String
getHint (Icon { hint }) =
    hint



-- primitives


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
            [ HtmlAttr.class (prefix ++ " fa-" ++ icon)
            , HtmlAttr.title hintText
            ]
            []
        )
