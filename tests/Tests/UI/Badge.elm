module Tests.UI.Badge exposing (tests)

import Fuzz
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Tests.Utils.Element exposing (elementToHtml, hasIconInside)
import UI.Badge as Badge


tests =
    describe "UI.Badge"
        [ fuzz Fuzz.string "sets the text content" <|
            \val ->
                Badge.light val
                    |> elementToHtml
                    |> Query.has [ Selector.text val ]
        ]
