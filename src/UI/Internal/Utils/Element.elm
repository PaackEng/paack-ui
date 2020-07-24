module UI.Internal.Utils.Element exposing (style, tuplesToStyles)

import Element exposing (Attribute)
import Html.Attributes as HtmlAttrs


style : String -> String -> Attribute msg
style k v =
    Element.htmlAttribute <| HtmlAttrs.style k v


tuplesToStyles : ( String, String ) -> Attribute msg
tuplesToStyles ( k, v ) =
    Element.htmlAttribute <| HtmlAttrs.style k v
