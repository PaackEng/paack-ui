module Tests.UI.ActionBar exposing (tests)

import Expect
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Tests.Utils.Element exposing (elementToHtml, hasIconInside)
import Tests.Utils.ExtraSelectors as ExtraSelectors
import UI.ActionBar as ActionBar
import UI.Button as Button


type Msg
    = OnCloseButtonClicked
    | TheOtherButtonClicked


tests : Test
tests =
    describe "UI.ActionBar"
        [ test "show the title when is set" <|
            \_ ->
                let
                    title =
                        "Hello, I'm a title"
                in
                ActionBar.actionBar
                    |> ActionBar.withTitle title
                    |> ActionBar.toEl
                    |> elementToHtml
                    |> Query.has [ Selector.text title ]
        , test "show the subtitle when is set" <|
            \_ ->
                let
                    subtitle =
                        "Hello, I'm a subtitle"
                in
                ActionBar.actionBar
                    |> ActionBar.withSubtitle subtitle
                    |> ActionBar.toEl
                    |> elementToHtml
                    |> Query.has [ Selector.text subtitle ]
        , test "shows the close button when is set" <|
            \_ ->
                ActionBar.actionBar
                    |> ActionBar.withCloseButton OnCloseButtonClicked
                    |> ActionBar.toEl
                    |> elementToHtml
                    |> Query.has
                        [ ExtraSelectors.button
                        , Selector.containing [ ExtraSelectors.closeIcon ]
                        ]
        , test "fires OnClosedButtonClicked message when the close button is clicked" <|
            \_ ->
                ActionBar.actionBar
                    |> ActionBar.withCloseButton OnCloseButtonClicked
                    |> ActionBar.toEl
                    |> elementToHtml
                    |> Query.find
                        [ ExtraSelectors.button
                        , Selector.containing [ ExtraSelectors.closeIcon ]
                        ]
                    |> Event.simulate Event.click
                    |> Event.expect OnCloseButtonClicked
        , test "shows an extra button when add buttons " <|
            \_ ->
                let
                    buttonLabel =
                        "Add new something"
                in
                ActionBar.actionBar
                    |> ActionBar.withButtons
                        [ Button.button TheOtherButtonClicked
                            |> Button.withPrimaryColor
                            |> Button.withText buttonLabel
                        ]
                    |> ActionBar.toEl
                    |> elementToHtml
                    |> Query.has (ExtraSelectors.textButton buttonLabel)
        ]
