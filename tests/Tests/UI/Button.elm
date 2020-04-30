module Tests.UI.Button exposing (tests)

import Element as El
import Expect
import Html.Attributes as HtmlAttr
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Tests.Utils.Element exposing (elementToHtml, hasIconInside)
import UI.Button as B
import UI.Icons as Icons
import UI.RenderConfig


type Msg
    = OnButtonClicked
    | OnToggleChanged Bool


fakeRenderConfig : UI.RenderConfig.RenderConfig
fakeRenderConfig =
    UI.RenderConfig.fromWindow
        { width = 1920
        , height = 1080
        }


tests : Test
tests =
    describe "UI.Button tests"
        [ withTextTests
        , withIconTests
        , disabledTests
        , eventsTests
        ]


withIconTests : Test
withIconTests =
    let
        iconHintText =
            "Toggle"
    in
    describe "#withIcon"
        [ test "sets the content with an icon" <|
            \_ ->
                B.bodyIcon Icons.toggle
                    |> B.button OnButtonClicked
                    |> B.withTone B.tonePrimary
                    |> B.toEl fakeRenderConfig
                    |> elementToHtml
                    |> findBtnQuery
                    |> hasIconInside iconHintText
        ]


withTextTests : Test
withTextTests =
    describe "#withText"
        [ test "sets the content with text" <|
            \_ ->
                clickableBtn
                    |> Query.has [ Selector.text "Click Here" ]
        ]


disabledTests : Test
disabledTests =
    describe "#withDisabledMode"
        [ describe "when withDisabledMode is set to True"
            [ test "Button doesn't fire message on click" <|
                \_ ->
                    B.bodyText "Click Here"
                        |> B.button OnButtonClicked
                        |> B.withMode B.modeDisabled
                        |> B.toEl fakeRenderConfig
                        |> elementToHtml
                        |> findBtnQuery
                        |> Event.simulate Event.click
                        |> Event.toResult
                        |> Expect.err
            , test "Button has disabled attribute" <|
                \_ ->
                    B.bodyText "Click Here"
                        |> B.button OnButtonClicked
                        |> B.withMode B.modeDisabled
                        |> B.toEl fakeRenderConfig
                        |> elementToHtml
                        |> Query.has [ Selector.attribute (HtmlAttr.attribute "disabled" "true") ]
            , test "Toggle doesn't fires message on click" <|
                \_ ->
                    B.bodyText "Click Here"
                        |> B.toggle OnToggleChanged False
                        |> B.withMode B.modeDisabled
                        |> B.toEl fakeRenderConfig
                        |> elementToHtml
                        |> findBtnQuery
                        |> Event.simulate Event.click
                        |> Event.toResult
                        |> Expect.err
            ]
        , describe "when withDisabledMode is set to False"
            [ test "Button fires OnButtonClicked message on click" <|
                \_ ->
                    B.bodyText "Click Here"
                        |> B.button OnButtonClicked
                        |> B.withMode B.modeEnabled
                        |> B.toEl fakeRenderConfig
                        |> elementToHtml
                        |> findBtnQuery
                        |> Event.simulate Event.click
                        |> Event.expect OnButtonClicked
            , test "Toogle fires OnButtonClicked message on click" <|
                \_ ->
                    B.bodyText "Click Here"
                        |> B.toggle OnToggleChanged False
                        |> B.toEl fakeRenderConfig
                        |> elementToHtml
                        |> findBtnQuery
                        |> Event.simulate Event.click
                        |> Event.expect (OnToggleChanged True)
            ]
        ]


eventsTests : Test
eventsTests =
    describe "Events"
        [ describe "#button"
            [ test "Click fires the OnButtonClicked" <|
                \_ ->
                    clickableBtn
                        |> Event.simulate Event.click
                        |> Event.expect OnButtonClicked
            ]
        , describe "#toggle"
            [ test "Click fires the (OnToggleChanged True) when is Off" <|
                \_ ->
                    B.bodyText "Click Here"
                        |> B.toggle OnToggleChanged False
                        |> B.toEl fakeRenderConfig
                        |> elementToHtml
                        |> findBtnQuery
                        |> Event.simulate Event.click
                        |> Event.expect (OnToggleChanged True)
            , test "Click fires the (OnToggleChanged False) when is On" <|
                \_ ->
                    B.bodyText "Click Here"
                        |> B.toggle OnToggleChanged True
                        |> B.toEl fakeRenderConfig
                        |> elementToHtml
                        |> findBtnQuery
                        |> Event.simulate Event.click
                        |> Event.expect (OnToggleChanged False)
            ]
        , describe "#link"
            [ test "Click do not fires the any Message" <|
                \_ ->
                    B.bodyText "Click Here"
                        |> B.link "http://www.google.com"
                        |> B.toEl fakeRenderConfig
                        |> elementToHtml
                        |> findBtnQuery
                        |> Event.simulate Event.click
                        |> Event.toResult
                        |> Expect.err
            , test "has href attribute" <|
                \_ ->
                    let
                        url =
                            "http://www.google.com"
                    in
                    B.bodyText "Click Here"
                        |> B.link url
                        |> B.toEl fakeRenderConfig
                        |> elementToHtml
                        |> Query.has [ Selector.attribute (HtmlAttr.attribute "href" url) ]
            ]
        ]


findBtnQuery : Query.Single Msg -> Query.Single Msg
findBtnQuery item =
    Query.find [ Selector.attribute (HtmlAttr.attribute "role" "button") ] item


clickableBtn : Query.Single Msg
clickableBtn =
    B.bodyText "Click Here"
        |> B.button OnButtonClicked
        |> B.toEl fakeRenderConfig
        |> elementToHtml
        |> findBtnQuery
