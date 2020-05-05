module UI.Icon exposing
    ( Icon
    , close
    , toEl
    , todo
    , toggle
    )

import Element exposing (..)
import Html
import Html.Attributes as HtmlAttr
import UI.Attributes exposing (title)
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
    = Toggle
    | Close


toggle : String -> Icon
toggle hint =
    Icon (Properties hint Toggle)


close : String -> Icon
close hint =
    Icon (Properties hint Close)


toEl : RenderConfig -> Icon -> Element msg
toEl _ (Icon { hint, glyph }) =
    case glyph of
        Toggle ->
            fasIcon "map" hint

        Close ->
            fasIcon "times" hint



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
