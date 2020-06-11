module UI.Utils.Element exposing
    ( RectangleSides
    , colorSetOpacity
    , colorTransition
    , desktopMaximum
    , disabled
    , ellipsis
    , maxHeightPct
    , maxHeightVH
    , onEnterPressed
    , svg
    , title
    , zeroPadding
    )

import Element exposing (Attribute, Element)
import Html.Attributes as HtmlAttrs
import Html.Events as HtmlEvents
import Json.Decode as Decode
import Svg
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Utils.ARIA as ARIA


type alias RectangleSides =
    { top : Int
    , left : Int
    , right : Int
    , bottom : Int
    }


svg : String -> List (Svg.Attribute msg) -> List (Svg.Svg msg) -> Element msg
svg altText attrs children =
    children
        |> Svg.svg attrs
        |> Element.html
        |> Element.el [ ARIA.roleAttr ARIA.roleImage, ARIA.labelAttr altText ]


disabled : List (Attribute msg)
disabled =
    [ HtmlAttrs.attribute "disabled" "true"
    , HtmlAttrs.attribute "aria-disabled" "true"
    , HtmlAttrs.attribute "tabindex" "-1"
    , HtmlAttrs.attribute "pointer-events" "none"
    , HtmlAttrs.style "cursor" "default"
    ]
        |> List.map Element.htmlAttribute


colorTransition : Int -> List (Attribute msg)
colorTransition time =
    [ ( "transition-property", "color, background-color, border-color" )
    , ( "transition-duration", String.fromInt time ++ "ms" )
    , ( "transition-timing-function", "linear" )
    ]
        |> List.map stylePair


onEnterPressed : msg -> Attribute msg
onEnterPressed msg =
    Element.htmlAttribute
        (HtmlEvents.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


title : String -> Attribute msg
title value =
    value
        |> HtmlAttrs.title
        |> Element.htmlAttribute


maxHeightVH : Int -> Attribute msg
maxHeightVH value =
    style "max-height" (String.fromInt value ++ "vh")


maxHeightPct : Float -> Attribute msg
maxHeightPct value =
    style "max-height" (String.fromFloat value ++ "%")


colorSetOpacity : Float -> Element.Color -> Element.Color
colorSetOpacity alpha color =
    color
        |> Element.toRgb
        |> (\rgba ->
                { rgba | alpha = alpha }
           )
        |> Element.fromRgb


ellipsis : List (Attribute msg)
ellipsis =
    [ ( "text-overflow", "ellipsis" )
    , ( "white-space", "nowrap" )
    ]
        |> List.map stylePair
        |> (::) Element.clip


desktopMaximum : RenderConfig -> Int -> Element.Length
desktopMaximum cfg maxSize =
    if RenderConfig.isMobile cfg then
        Element.fill

    else
        Element.fill |> Element.maximum maxSize


zeroPadding : RectangleSides
zeroPadding =
    { top = 0
    , left = 0
    , right = 0
    , bottom = 0
    }



-- Helpers


style : String -> String -> Attribute msg
style k v =
    Element.htmlAttribute <| HtmlAttrs.style k v


stylePair : ( String, String ) -> Attribute msg
stylePair ( k, v ) =
    Element.htmlAttribute <| HtmlAttrs.style k v
