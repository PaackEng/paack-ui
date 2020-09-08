module Tests.UI.Tabs exposing (tests)

import Expect exposing (Expectation)
import Html.Attributes as HtmlAttr
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Tests.Utils.Element exposing (elementToHtml)
import Tests.Utils.ExtraEvents as ExtraEvents
import Tests.Utils.RenderConfig exposing (desktopWindowConfig)
import UI.Tabs as Tabs


type Msg
    = SelectTab SelectTab


type SelectTab
    = TabOne
    | TabTwo
    | TabThree
    | TabFour
    | TabFive


tests : Test
tests =
    describe "UI.Tabs"
        [ tabindexTests
        , selectionTests
        , eventsTests
        ]


tabindexTests : Test
tabindexTests =
    let
        eachTabElement =
            findTabs tabsElement
    in
    describe "#tabIndex"
        [ test "has exactly 5 tabs" <|
            \_ ->
                eachTabElement
                    |> Query.count (Expect.equal 5)
        , test "them all have tabindex" <|
            \_ ->
                eachTabElement
                    |> Query.each hasTabIndex
        ]


selectionTests : Test
selectionTests =
    describe "#ariaSelected"
        [ test "has exactly 4 not selected tabs" <|
            \_ ->
                tabsElement
                    |> findSelected False
                    |> Query.count (Expect.equal 4)
        , test "has exactly 1 selected tab" <|
            \_ ->
                tabsElement
                    |> findSelected True
                    |> Query.count (Expect.equal 1)
        , test "the first one is the selected" <|
            \_ ->
                tabsElement
                    |> findTabs
                    |> Query.first
                    |> Query.has
                        [ Selector.attribute (HtmlAttr.attribute "aria-selected" "true") ]
        ]


eventsTests : Test
eventsTests =
    describe "Events"
        [ test "Click on the first does nothing" <|
            \_ ->
                tabsElement
                    |> findTabs
                    |> Query.first
                    |> Event.simulate Event.click
                    |> Event.toResult
                    |> Expect.err
        , test "Enter on the second fires \"SelectTab TabTwo\"" <|
            \_ ->
                tabsElement
                    |> findTabs
                    |> Query.index 1
                    |> Event.simulate ExtraEvents.enterKey
                    |> Event.toResult
                    -- I've used toResult cause it show proper error messages
                    |> Expect.equal (Ok (SelectTab TabTwo))
        , test "Click on the fourth fires \"SelectTab TabFour\"" <|
            \_ ->
                tabsElement
                    |> findTabs
                    |> Query.index 3
                    |> Event.simulate Event.click
                    |> Event.toResult
                    |> Expect.equal (Ok (SelectTab TabFour))
        ]



-- Helpers


tabsElement : Query.Single Msg
tabsElement =
    Tabs.tabList SelectTab
        (always "FILL")
        [ TabOne, TabTwo, TabThree, TabFour, TabFive ]
        TabOne
        |> Tabs.renderElement desktopWindowConfig
        |> elementToHtml


hasTabIndex : Query.Single msg -> Expectation
hasTabIndex original =
    -- Note: elm-ui has left the I in uppercase
    Query.has
        [ Selector.attribute (HtmlAttr.attribute "tabIndex" "0") ]
        original


findSelected : Bool -> Query.Single Msg -> Query.Multiple Msg
findSelected comparing original =
    let
        boolStr =
            if comparing then
                "true"

            else
                "false"
    in
    Query.findAll
        [ Selector.attribute (HtmlAttr.attribute "aria-selected" boolStr) ]
        original


findTabs : Query.Single Msg -> Query.Multiple Msg
findTabs item =
    Query.findAll
        [ Selector.attribute (HtmlAttr.attribute "role" "tab") ]
        item
