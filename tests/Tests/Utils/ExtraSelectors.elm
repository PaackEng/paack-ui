module Tests.Utils.ExtraSelectors exposing (hasAttribute)

import Html.Attributes as HtmlAttrs
import Test.Html.Selector as Selector


hasAttribute : String -> String -> Selector.Selector
hasAttribute attribute val =
    Selector.attribute (HtmlAttrs.attribute attribute val)
