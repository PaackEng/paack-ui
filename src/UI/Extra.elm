module UI.Extra exposing (elAsModal, scrollableYEl)

import Element exposing (..)
import UI.Attributes exposing (style)


elAsModal : List (Attribute msg) -> Element msg -> Element msg
elAsModal attrs content =
    el
        (attrs
            ++ [ style "position" "fixed"
               , style "width" "100%"
               , style "height" "100%"
               , style "top" "0"
               , style "left" "0"
               , style "z-index" "1000"
               ]
        )
        content



-- Reference: https://github.com/mdgriffith/elm-ui/issues/70


scrollableYEl : List (Attribute msg) -> Element msg -> Element msg
scrollableYEl attrs body =
    el
        ([ style "position" "absolute"
         , style "top" "0"
         , style "right" "0"
         , style "bottom" "0"
         , style "left" "0"
         , scrollbarY
         ]
            ++ attrs
        )
        body
