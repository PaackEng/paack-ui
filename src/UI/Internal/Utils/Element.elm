module UI.Internal.Utils.Element exposing (..)

import Element exposing (Attribute)
import Html.Attributes as HtmlAttrs


style : String -> String -> Attribute msg
style k v =
    Element.htmlAttribute <| HtmlAttrs.style k v


tuplesToStyles : ( String, String ) -> Attribute msg
tuplesToStyles ( k, v ) =
    Element.htmlAttribute <| HtmlAttrs.style k v


ellipsisAttrs : Int -> String -> List (Attribute msg)
ellipsisAttrs lineHeightSize titleContent =
    [ ( "text-overflow", "ellipsis" )
    , ( "white-space", "nowrap" )
    , ( "overflow", "hidden" )
    , ( "display", "block" )
    , ( "line-height", String.fromInt lineHeightSize ++ "px" )
    ]
        |> List.map tuplesToStyles
        |> (::) Element.clip
        |> (::) (title titleContent)


title : String -> Attribute msg
title value =
    value
        |> HtmlAttrs.title
        |> Element.htmlAttribute



-- Some good-old CSS


positionFixed : Attribute msg
positionFixed =
    Element.htmlAttribute <| HtmlAttrs.style "position" "fixed"


zIndex : Int -> Attribute msg
zIndex val =
    Element.htmlAttribute <| HtmlAttrs.style "z-index" (String.fromInt val)
