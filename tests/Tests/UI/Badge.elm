module Tests.UI.Badge exposing (tests)

import Fuzz
import Test exposing (Test, describe, fuzz)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Tests.Utils.Element exposing (elementToHtml)
import Tests.Utils.RenderConfig exposing (desktopWindowConfig)
import UI.Badge as Badge


tests : Test
tests =
    describe "UI.Badge"
        [ fuzz Fuzz.string "sets the text content" <|
            \val ->
                Badge.primaryDark val
                    |> Badge.renderElement desktopWindowConfig
                    |> elementToHtml
                    |> Query.has [ Selector.text val ]
        ]
