module Tests.Utils.Element exposing (cursorPointer, elementToHtml, hasIconInside)

import Element as El
import Expect exposing (Expectation)
import Html.Attributes as HtmlAttr
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)


elementToHtml : El.Element msg -> Query.Single msg
elementToHtml content =
    content
        |> El.layout []
        |> Query.fromHtml


hasIconInside : String -> Query.Single msg -> Expectation
hasIconInside hintText content =
    content
        |> Query.has
            [ Selector.containing
                [ Selector.attribute
                    (HtmlAttr.attribute "role" "img")
                , Selector.attribute
                    (HtmlAttr.attribute "aria-label" hintText)
                , Selector.containing
                    [ Selector.tag "i" ]
                ]
            ]


cursorPointer : Selector
cursorPointer =
    Selector.class "cptr"
