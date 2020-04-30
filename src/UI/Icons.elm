module UI.Icons exposing
    ( Icon
    , checked
    , chevronDown
    , chevronLeft
    , chevronRight
    , chevronUp
    , close
    , collapse
    , expand
    , filter
    , map
    , mapPoint
    , messages
    , more
    , move
    , notepad
    , person
    , plus
    , reload
    , route
    , search
    , settings
    , spinner
    , toEl
    , toggle
    , trash
    , unchecked
    , warning
    )

import Element exposing (..)
import Html
import Html.Attributes as HtmlAttr
import UI.Attributes exposing (title)
import UI.RenderConfig exposing (RenderConfig)



-- NEW


type Icon
    = Toggle


toggle : Icon
toggle =
    Toggle


toEl : RenderConfig -> Icon -> Element msg
toEl _ ico =
    case ico of
        Toggle ->
            fasIcon "map" "Toggle"



-- OLD


spinner : Element msg
spinner =
    fasIcon "spinner fa-spin" ""


chevronDown : String -> Element msg
chevronDown hintText =
    fasIcon "chevron-down" hintText


chevronUp : String -> Element msg
chevronUp hintText =
    fasIcon "chevron-up" hintText


chevronLeft : String -> Element msg
chevronLeft hintText =
    fasIcon "chevron-left" hintText


chevronRight : String -> Element msg
chevronRight hintText =
    fasIcon "chevron-right" hintText


checked : String -> Element msg
checked hintText =
    fasIcon "check-square" hintText


close : String -> Element msg
close hintText =
    fasIcon "times" hintText


collapse : String -> Element msg
collapse hintText =
    fasIcon "compress-alt" hintText


expand : String -> Element msg
expand hintText =
    fasIcon "expand-alt" hintText


filter : String -> Element msg
filter hintText =
    fasIcon "filter" hintText


map : String -> Element msg
map hintText =
    fasIcon "map" hintText


mapPoint : String -> Element msg
mapPoint hintText =
    fasIcon "map-marker" hintText


messages : String -> Element msg
messages hintText =
    fasIcon "comment" hintText


more : String -> Element msg
more hintText =
    fasIcon "ellipsis-h" hintText


move : String -> Element msg
move hintText =
    fasIcon "exchange-alt" hintText


notepad : String -> Element msg
notepad hintText =
    fasIcon "clipboard-check" hintText


person : String -> Element msg
person hintText =
    fasIcon "user" hintText


plus : String -> Element msg
plus hintText =
    fasIcon "plus" hintText


reload : String -> Element msg
reload hintText =
    fasIcon "sync" hintText


route : String -> Element msg
route hintText =
    fasIcon "route" hintText


search : String -> Element msg
search hintText =
    fasIcon "search" hintText


settings : String -> Element msg
settings hintText =
    fasIcon "cog" hintText


trash : String -> Element msg
trash hintText =
    fasIcon "trash" hintText


unchecked : String -> Element msg
unchecked hintText =
    farIcon "square" hintText


warning : String -> Element msg
warning hintText =
    fasIcon "exclamation-triangle" hintText



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
