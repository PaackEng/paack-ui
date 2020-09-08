module Tests.Utils.ExtraSelectors exposing
    ( button
    , checkbox
    , closeIcon
    , hasAttribute
    , isDisabled
    , loadingView
    , plusIcon
    , textButton
    )

import Html.Attributes as HtmlAttrs
import Test.Html.Selector as Selector


hasAttribute : String -> String -> Selector.Selector
hasAttribute attribute val =
    Selector.attribute (HtmlAttrs.attribute attribute val)


isDisabled : Selector.Selector
isDisabled =
    Selector.attribute (HtmlAttrs.attribute "aria-disabled" "true")


closeIcon : Selector.Selector
closeIcon =
    iconSelector "Close1"


plusIcon : Selector.Selector
plusIcon =
    iconSelector "plus"


button : Selector.Selector
button =
    hasAttribute "role" "button"


textButton : String -> List Selector.Selector
textButton label =
    [ hasAttribute "role" "button"
    , Selector.containing
        [ Selector.text label ]
    ]


iconSelector : String -> Selector.Selector
iconSelector icon =
    Selector.id icon


loadingView : Selector.Selector
loadingView =
    Selector.id "loading-view"


checkbox : Selector.Selector
checkbox =
    hasAttribute "role" "checkbox"
