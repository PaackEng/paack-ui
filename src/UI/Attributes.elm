module UI.Attributes exposing
    ( ariaChecked
    , ariaDisabled
    , ariaLabel
    , ariaRole
    , borderBottomWidth
    , borderLeftWidth
    , borderRightWidth
    , borderTopWidth
    , custom
    , onClickWithoutPropagation
    , paddingBottom
    , paddingLeft
    , paddingRight
    , paddingTop
    , style
    , title
    , transition
    )

import Element exposing (..)
import Element.Border as Border
import Helpers exposing (ifThenElse)
import Html.Attributes as HtmlAttrs
import Html.Events as HtmlEvents
import Json.Decode as Decode


custom : String -> String -> Attribute msg
custom key val =
    htmlAttribute (HtmlAttrs.attribute key val)


ariaRole : String -> Attribute msg
ariaRole val =
    htmlAttribute (HtmlAttrs.attribute "role" val)


ariaChecked : Bool -> Attribute msg
ariaChecked flag =
    let
        checkVal =
            ifThenElse flag "true" "false"
    in
    htmlAttribute (HtmlAttrs.attribute "aria-checked" checkVal)


ariaDisabled : Bool -> Attribute msg
ariaDisabled flag =
    let
        checkVal =
            ifThenElse flag "true" "false"
    in
    htmlAttribute (HtmlAttrs.attribute "aria-disabled" checkVal)


ariaLabel : String -> Attribute msg
ariaLabel val =
    htmlAttribute (HtmlAttrs.attribute "aria-label" val)


title : String -> Attribute msg
title val =
    htmlAttribute (HtmlAttrs.attribute "title" val)


borderRightWidth : Int -> Attribute msg
borderRightWidth size =
    Border.widthEach
        { defaultSides | right = size }


borderLeftWidth : Int -> Attribute msg
borderLeftWidth size =
    Border.widthEach
        { defaultSides | left = size }


borderTopWidth : Int -> Attribute msg
borderTopWidth size =
    Border.widthEach
        { defaultSides | top = size }


borderBottomWidth : Int -> Attribute msg
borderBottomWidth size =
    Border.widthEach
        { defaultSides | bottom = size }


paddingLeft : Int -> Attribute msg
paddingLeft size =
    paddingEach
        { defaultSides | left = size }


paddingRight : Int -> Attribute msg
paddingRight size =
    paddingEach
        { defaultSides | right = size }


paddingTop : Int -> Attribute msg
paddingTop size =
    paddingEach
        { defaultSides | top = size }


paddingBottom : Int -> Attribute msg
paddingBottom size =
    paddingEach
        { defaultSides | bottom = size }


defaultSides : { top : Int, right : Int, left : Int, bottom : Int }
defaultSides =
    { top = 0
    , right = 0
    , left = 0
    , bottom = 0
    }


style : String -> String -> Attribute msg
style key value =
    htmlAttribute (HtmlAttrs.style key value)


onClickWithoutPropagation : msg -> Attribute msg
onClickWithoutPropagation msg =
    htmlAttribute <|
        HtmlEvents.custom "click" (Decode.succeed { message = msg, stopPropagation = True, preventDefault = True })


transition : Int -> List String -> Attribute msg
transition duration properties =
    style
        "transition"
        (properties
            |> List.map (\prop -> prop ++ " " ++ String.fromInt duration ++ "ms ease-in-out")
            |> String.join ", "
        )
