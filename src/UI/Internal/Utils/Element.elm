module UI.Internal.Utils.Element exposing
    ( clickElsewhereToLeave
    , css
    , id
    , overflowAttrs
    , overflowVisible
    , overlay
    , shrinkButClip
    , style
    , tabIndex
    , title
    , tuplesToStyles
    , zIndex
    )

import Element exposing (Attribute, Element, fill, minimum, shrink)
import Element.Events as Events
import Html.Attributes as HtmlAttrs
import UI.Internal.Colors as Colors


style : String -> String -> Attribute msg
style k v =
    Element.htmlAttribute <| HtmlAttrs.style k v


css : List ( String, String ) -> List (Attribute msg)
css =
    List.map tuplesToStyles


tuplesToStyles : ( String, String ) -> Attribute msg
tuplesToStyles ( k, v ) =
    Element.htmlAttribute <| HtmlAttrs.style k v


overflowAttrs : Int -> List ( String, String ) -> String -> List (Attribute msg)
overflowAttrs lineHeightSize attrs titleContent =
    attrs
        |> (::) ( "display", "block" )
        |> (::) ( "line-height", String.fromInt lineHeightSize ++ "px" )
        |> List.map tuplesToStyles
        |> (::) Element.clip
        |> (::) (title titleContent)


title : String -> Attribute msg
title value =
    value
        |> HtmlAttrs.title
        |> Element.htmlAttribute


id : String -> Attribute msg
id value =
    value
        |> HtmlAttrs.id
        |> Element.htmlAttribute


clickElsewhereToLeave : msg -> List (Attribute msg) -> Element msg -> Element msg
clickElsewhereToLeave onClickMsg backgroundStyleAttributes foregroundContent =
    let
        backgroundAttributes =
            [ positionFixed -- Needs for starting at the top-left corner
            , zIndex 8
            , Element.htmlAttribute <| HtmlAttrs.style "top" "0"
            , Element.htmlAttribute <| HtmlAttrs.style "left" "0"
            , Element.htmlAttribute <| HtmlAttrs.style "width" "100vw"
            , Element.htmlAttribute <| HtmlAttrs.style "height" "100vh"
            , Events.onClick onClickMsg
            ]
                ++ backgroundStyleAttributes
    in
    Element.el
        [ Element.width fill
        , Element.height (shrink |> minimum 1)
        , Element.inFront foregroundContent
        ]
        (Element.el backgroundAttributes Element.none)


overlay : msg -> Element msg -> Element msg
overlay closeMsg content =
    clickElsewhereToLeave closeMsg [ Colors.overlayBackground ] content


shrinkButClip : List (Attribute msg)
shrinkButClip =
    List.map tuplesToStyles
        [ ( "width", "min-content" )
        , ( "max-width", "100%" )
        , ( "overflow", "clip" )
        ]



-- Some good-old CSS


positionFixed : Attribute msg
positionFixed =
    Element.htmlAttribute <| HtmlAttrs.style "position" "fixed"


overflowVisible : Attribute msg
overflowVisible =
    Element.htmlAttribute <| HtmlAttrs.style "overflow" "visible"


zIndex : Int -> Attribute msg
zIndex val =
    Element.htmlAttribute <| HtmlAttrs.style "z-index" (String.fromInt val)


{-| Give preference to UI.Util.Focus
-}
tabIndex : Int -> Attribute msg
tabIndex val =
    Element.htmlAttribute <| HtmlAttrs.style "tabIndex" (String.fromInt val)
