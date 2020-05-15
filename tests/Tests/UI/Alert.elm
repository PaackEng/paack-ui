module Tests.UI.Alert exposing (tests)

import Fuzz
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Tests.Utils.Element exposing (elementToHtml)
import Tests.Utils.ExtraSelectors as ExtraSelectors
import Tests.Utils.RenderConfig exposing (desktopWindowConfig)
import UI.Alert as Alert exposing (Alert)


type Msg
    = OnCloseBtnClicked


tests : Test
tests =
    describe "UI.Alert"
        [ testCase "primary" Alert.primary
        , testCase "success" Alert.success
        , testCase "danger" Alert.danger
        , testCase "warning" Alert.warning
        ]


testCase : String -> (String -> Alert msg) -> Test
testCase alertType alertFn =
    describe alertType
        [ fuzz Fuzz.string "shows the title when it is set" <|
            \title ->
                alertFn title
                    |> Alert.toEl desktopWindowConfig
                    |> elementToHtml
                    |> Query.has [ Selector.text title ]
        ]
