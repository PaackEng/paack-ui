module UI.Internal.Utils.Element exposing (ellipsisAttrs, style, title, tuplesToStyles)

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
