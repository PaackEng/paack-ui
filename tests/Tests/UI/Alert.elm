module Tests.UI.Alert exposing (tests)

import Fuzz
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Tests.Utils.Element exposing (elementToHtml)
import Tests.Utils.ExtraSelectors as ExtraSelectors
import UI.Alert as Alert


type Msg
    = OnCloseBtnClicked


tests : Test
tests =
    describe "UI.Alert"
        [ fuzz Fuzz.string "shows the title when it is set" <|
            \title ->
                Alert.info
                    |> Alert.withTitle title
                    |> Alert.toEl
                    |> elementToHtml
                    |> Query.has [ Selector.text title ]
        , fuzz Fuzz.string "shows the subtitle when it is set" <|
            \subtitle ->
                Alert.info
                    |> Alert.withSubtitle subtitle
                    |> Alert.toEl
                    |> elementToHtml
                    |> Query.has [ Selector.text subtitle ]
        , test "shows the close button when the message is set" <|
            \_ ->
                Alert.info
                    |> Alert.withCloseButton OnCloseBtnClicked
                    |> Alert.toEl
                    |> elementToHtml
                    |> Query.has [ ExtraSelectors.closeIcon ]
        , test "fires the close button message when the button is clicked" <|
            \_ ->
                Alert.info
                    |> Alert.withCloseButton OnCloseBtnClicked
                    |> Alert.toEl
                    |> elementToHtml
                    |> Query.find
                        [ ExtraSelectors.button
                        , Selector.containing [ ExtraSelectors.closeIcon ]
                        ]
                    |> Event.simulate Event.click
                    |> Event.expect OnCloseBtnClicked
        ]
