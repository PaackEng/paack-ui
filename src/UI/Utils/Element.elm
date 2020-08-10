module UI.Utils.Element exposing
    ( colorSetOpacity, colorTransition
    , desktopMaximum
    , svg, title, maxHeightVH, maxHeightPct
    , disabled, onEnterPressed, onIndividualClick
    , RectangleSides, zeroPadding
    )

{-| Utilities and functionality that are not covered by Elm UI.


# Color

@docs colorSetOpacity, colorTransition


# Responsiveness

@docs desktopMaximum


# HTML features

@docs svg, title, maxHeightVH, maxHeightPct


# Input

@docs disabled, onEnterPressed, onIndividualClick


# Padding, borders and size

@docs RectangleSides, zeroPadding

-}

import Element exposing (Attribute, Element)
import Html.Attributes as HtmlAttrs
import Html.Events as HtmlEvents
import Json.Decode as Decode
import Svg
import UI.Internal.Utils.Element as Internal exposing (style, tuplesToStyles)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Utils.ARIA as ARIA


{-| The classic top, left, right and bottom as integers record.
-}
type alias RectangleSides =
    { top : Int
    , left : Int
    , right : Int
    , bottom : Int
    }


{-| The SVG element, with ARIA attributes applied.

    svg "Alt text" svgAttributes svgContent

-}
svg : String -> List (Svg.Attribute msg) -> List (Svg.Svg msg) -> Element msg
svg altText attrs children =
    children
        |> Svg.svg attrs
        |> Element.html
        |> Element.el (ARIA.toElementAttributes <| ARIA.roleImage altText)


{-| All the attributes that define an element as disabled for modifying.

    Element.el Element.disabled <| Element.text "Some content"

-}
disabled : List (Attribute msg)
disabled =
    [ HtmlAttrs.attribute "disabled" "true"
    , HtmlAttrs.attribute "aria-disabled" "true"
    , HtmlAttrs.attribute "tabindex" "-1"
    , HtmlAttrs.attribute "pointer-events" "none"
    , HtmlAttrs.style "cursor" "default"
    ]
        |> List.map Element.htmlAttribute


{-| Enable CSS transition for HTML's `color, background-color, border-color`.
Time is set in miliseconds.

    Element.el
        (Background.color someVariatingColor :: Element.colorTransition 200)
        someChildElement

-}
colorTransition : Int -> List (Attribute msg)
colorTransition time =
    [ ( "transition-property", "color, background-color, border-color" )
    , ( "transition-duration", String.fromInt time ++ "ms" )
    , ( "transition-timing-function", "linear" )
    ]
        |> List.map tuplesToStyles


{-| Trigger message when the users press return-key while element is on-focus.

    Element.el [ Element.onEnterPressed Msg.ActivateSomething ] someChildElement

-}
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


{-| "The title attribute specifies extra information about an element.

The information is most often shown as a tooltip text when the mouse moves over the element." - W3Schools

    Element.el [ Element.title "Some text" ] someChildElement

-}
title : String -> Attribute msg
title value =
    Internal.title value


{-| Wrapper for CSS's `max-height: {{vaue}}vh`.

    Element.el [ Element.maxHeightVH 100 ] someChildElement

-}
maxHeightVH : Int -> Attribute msg
maxHeightVH value =
    style "max-height" (String.fromInt value ++ "vh")


{-| Wrapper for CSS's `max-height: {{vaue}}%`.

    Element.el [ Element.maxHeightPct 100 ] someChildElement

-}
maxHeightPct : Float -> Attribute msg
maxHeightPct value =
    style "max-height" (String.fromFloat value ++ "%")


{-| Overwrite alpha value in an [`Element.Color`](/packages/mdgriffith/elm-ui/latest/Element#Color).
-}
colorSetOpacity : Float -> Element.Color -> Element.Color
colorSetOpacity alpha color =
    color
        |> Element.toRgb
        |> (\rgba ->
                { rgba | alpha = alpha }
           )
        |> Element.fromRgb


{-| Limit [`Element.fill`](/packages/mdgriffith/elm-ui/latest/Element#fill) only when on desktop.

    Element.width (Element.desktopMaximum 640)

-}
desktopMaximum : RenderConfig -> Int -> Element.Length
desktopMaximum cfg maxSize =
    if RenderConfig.isMobile cfg then
        Element.fill

    else
        Element.fill |> Element.maximum maxSize


{-| Zero-initialized record for paddings and borders.

    Element.paddingEach
        { zeroPadding | bottom = 20 }

-}
zeroPadding : RectangleSides
zeroPadding =
    { top = 0
    , left = 0
    , right = 0
    , bottom = 0
    }


{-| Equivalent to [`Element.onClick`](/packages/mdgriffith/elm-ui/latest/Element#fill) with `stopPropagation` and `preventDefault` applied.

    Element.el
        [ Element.onIndividualClick Msg.CoolTextClicked ]
        (Element.text "Cool text")

-}
onIndividualClick : msg -> Attribute msg
onIndividualClick message =
    Decode.succeed message
        |> Decode.map (\msg -> { message = msg, stopPropagation = True, preventDefault = True })
        |> HtmlEvents.custom "click"
        |> Element.htmlAttribute
