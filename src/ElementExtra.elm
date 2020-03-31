module ElementExtra exposing (empty, maybeText, maybeWhen, when)

import Element exposing (Element)


when : Bool -> Element msg -> Element msg
when shouldRender view =
    if shouldRender then
        view

    else
        Element.none


empty : Element msg
empty =
    Element.none


maybeWhen : Maybe (Element msg) -> Element msg
maybeWhen maybeView =
    case maybeView of
        Just view ->
            view

        Nothing ->
            Element.none


maybeText : String -> Maybe String -> Element msg
maybeText defaultString maybeString =
    maybeString
        |> Maybe.withDefault defaultString
        |> Element.text
