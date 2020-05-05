module Tests.UI.PopUp exposing (tests)

import Element as El
import Expect
import Html.Attributes as HtmlAttr
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Tests.Utils.Element exposing (elementToHtml)
import UI.Icon as Icons
import UI.PopUp as PopUp


type Msg
    = OnCloseClicked
    | OnCollapseClicked Bool


tests : Test
tests =
    describe "UI.PopUp tests"
        [ withTitleTests
        , withContentTests
        , withCollapsedTest
        , eventsTests
        ]


withTitleTests : Test
withTitleTests =
    let
        titleText =
            "PopUp Title"
    in
    describe "#withTitle"
        [ test "if not used the title is empty" <|
            \_ ->
                PopUp.collapsable OnCollapseClicked
                    |> PopUp.toEl
                    |> elementToHtml
                    |> findPopUpHeaderQuery
                    |> Query.has [ Selector.text "" ]
        , test "sets the title of the pop up" <|
            \_ ->
                PopUp.collapsable OnCollapseClicked
                    |> PopUp.withTitle titleText
                    |> PopUp.toEl
                    |> elementToHtml
                    |> findPopUpHeaderQuery
                    |> Query.has [ Selector.text titleText ]
        ]


withContentTests : Test
withContentTests =
    let
        contentText =
            "PopUp Content"
    in
    describe "#withContent"
        [ test "if not used the content is none" <|
            \_ ->
                PopUp.collapsable OnCollapseClicked
                    |> PopUp.toEl
                    |> elementToHtml
                    |> findPopUpContentQuery
                    |> Query.children []
                    |> Query.count (Expect.equal 0)
        , test "sets the content of the pop up" <|
            \_ ->
                PopUp.collapsable OnCollapseClicked
                    |> PopUp.withContent (El.text contentText)
                    |> PopUp.withCollapsed False
                    |> PopUp.toEl
                    |> elementToHtml
                    |> findPopUpContentQuery
                    |> Query.has [ Selector.text contentText ]
        ]


withCollapsedTest : Test
withCollapsedTest =
    let
        contentText =
            "PopUp Content"
    in
    describe "#withCollapsed"
        [ test "if not used the content is collapsed by default" <|
            \_ ->
                PopUp.collapsable OnCollapseClicked
                    |> PopUp.withContent (El.text contentText)
                    |> PopUp.toEl
                    |> elementToHtml
                    |> findPopUpContentQuery
                    |> Query.children []
                    |> Query.count (Expect.equal 0)
        , test "sets to false shows the pop up's content" <|
            \_ ->
                PopUp.collapsable OnCollapseClicked
                    |> PopUp.withContent (El.text contentText)
                    |> PopUp.withCollapsed False
                    |> PopUp.toEl
                    |> elementToHtml
                    |> findPopUpContentQuery
                    |> Query.has [ Selector.text contentText ]
        , test "sets to true hides the pop up's content" <|
            \_ ->
                PopUp.collapsable OnCollapseClicked
                    |> PopUp.withContent (El.text contentText)
                    |> PopUp.withCollapsed True
                    |> PopUp.toEl
                    |> elementToHtml
                    |> findPopUpContentQuery
                    |> Query.children []
                    |> Query.count (Expect.equal 0)
        ]


eventsTests : Test
eventsTests =
    let
        contentText =
            "PopUp Content"
    in
    describe "Events"
        [ describe "collapsable"
            [ test "Click fires (OnCollapseClicked True) when collapsed is False" <|
                \_ ->
                    PopUp.collapsable OnCollapseClicked
                        |> PopUp.withContent (El.text contentText)
                        |> PopUp.withCollapsed False
                        |> PopUp.toEl
                        |> elementToHtml
                        |> findPopUpButtonQuery "pop-up-collapse"
                        |> Event.simulate Event.click
                        |> Event.expect (OnCollapseClicked True)
            , test "Click fires (OnCollapseClicked False) when collapsed is True" <|
                \_ ->
                    PopUp.collapsable OnCollapseClicked
                        |> PopUp.withContent (El.text contentText)
                        |> PopUp.withCollapsed True
                        |> PopUp.toEl
                        |> elementToHtml
                        |> findPopUpButtonQuery "pop-up-expand"
                        |> Event.simulate Event.click
                        |> Event.expect (OnCollapseClicked False)
            ]
        , describe "closable"
            [ test "Click fires OnCloseClicked" <|
                \_ ->
                    PopUp.closable OnCloseClicked
                        |> PopUp.withContent (El.text contentText)
                        |> PopUp.withCollapsed False
                        |> PopUp.toEl
                        |> elementToHtml
                        |> findPopUpButtonQuery "pop-up-close"
                        |> Event.simulate Event.click
                        |> Event.expect OnCloseClicked
            ]
        ]


findPopUpHeaderQuery : Query.Single Msg -> Query.Single Msg
findPopUpHeaderQuery item =
    Query.find
        [ Selector.attribute (HtmlAttr.attribute "aria-label" "pop-up-header")
        ]
        item


findPopUpButtonQuery : String -> Query.Single Msg -> Query.Single Msg
findPopUpButtonQuery label item =
    Query.find
        [ Selector.attribute (HtmlAttr.attribute "aria-label" label)
        ]
        item


findPopUpContentQuery : Query.Single Msg -> Query.Single Msg
findPopUpContentQuery item =
    Query.find
        [ Selector.attribute (HtmlAttr.attribute "aria-label" "pop-up-content")
        ]
        item
