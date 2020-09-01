module UI.Internal.Utils.Element exposing (..)

import Element exposing (Attribute, Element, fill, minimum, shrink)
import Element.Events as Events
import Html.Attributes as HtmlAttrs
import UI.Internal.Palette as Palette


style : String -> String -> Attribute msg
style k v =
    Element.htmlAttribute <| HtmlAttrs.style k v


tuplesToStyles : ( String, String ) -> Attribute msg
tuplesToStyles ( k, v ) =
    Element.htmlAttribute <| HtmlAttrs.style k v


overflowAttrs : Int -> List ( String, String ) -> String -> List (Attribute msg)
overflowAttrs lineHeightSize attrs titleContent =
    attrs
        |> (++)
            [ ( "display", "block" )
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


overlayBackground : msg -> Element msg
overlayBackground onClickMsg =
    Element.el
        [ positionFixed -- Needs for starting at the top-left corner
        , zIndex 8
        , Palette.overlayBackground
        , Element.htmlAttribute <| HtmlAttrs.style "top" "0"
        , Element.htmlAttribute <| HtmlAttrs.style "left" "0"
        , Element.htmlAttribute <| HtmlAttrs.style "width" "100vw"
        , Element.htmlAttribute <| HtmlAttrs.style "height" "100vh"
        , Events.onClick onClickMsg
        ]
        Element.none


overlay : msg -> Element msg -> Element msg
overlay closeMsg content =
    Element.el
        [ Element.width fill
        , Element.height (shrink |> minimum 1)
        , Element.inFront content
        ]
        (overlayBackground closeMsg)


tabIndex : Int -> Attribute msg
tabIndex code =
    -- -1 is disabled, 0 is following document flow, greater than 0 are priorities numbers
    String.fromInt code
        |> HtmlAttrs.attribute "tabindex"
        |> Element.htmlAttribute



-- Some good-old CSS


positionFixed : Attribute msg
positionFixed =
    Element.htmlAttribute <| HtmlAttrs.style "position" "fixed"


positionRelative : Attribute msg
positionRelative =
    Element.htmlAttribute <| HtmlAttrs.style "position" "relative"


positionAbsolute : Attribute ms
positionAbsolute =
    Element.htmlAttribute <| HtmlAttrs.style "position" "absolute"


positionAbsoluteTop : Attribute ms
positionAbsoluteTop =
    Element.htmlAttribute <| HtmlAttrs.style "top" "1"


positionAbsoluteLeft : Attribute ms
positionAbsoluteLeft =
    Element.htmlAttribute <| HtmlAttrs.style "left" "1"


overflowVisible : Attribute msg
overflowVisible =
    Element.htmlAttribute <| HtmlAttrs.style "overflow" "visible"


zIndex : Int -> Attribute msg
zIndex val =
    Element.htmlAttribute <| HtmlAttrs.style "z-index" (String.fromInt val)


borderTriangleUp : String -> Attribute msg
borderTriangleUp cssColor =
    style "border-color" ("transparent transparent " ++ cssColor ++ " transparent")
