module Tests.UI.Button exposing (tests)

import Element as El
import Expect
import Html.Attributes as HtmlAttr
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Tests.Utils.Element exposing (elementToHtml, hasIconInside)
import UI.Button as Button
import UI.Icons as Icon
import UI.Link as Link
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
                Button.bodyIcon (Icon.toggle iconHintText)
                    |> Button.button OnButtonClicked
                    |> Button.withTone Button.tonePrimary
                    |> Button.toEl fakeRenderConfig
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
                    Button.bodyText "Click Here"
                        |> Button.button OnButtonClicked
                        |> Button.withMode Button.modeDisabled
                        |> Button.toEl fakeRenderConfig
                        |> elementToHtml
                        |> findBtnQuery
                        |> Event.simulate Event.click
                        |> Event.toResult
                        |> Expect.err
            , test "Button has disabled attribute" <|
                \_ ->
                    Button.bodyText "Click Here"
                        |> Button.button OnButtonClicked
                        |> Button.withMode Button.modeDisabled
                        |> Button.toEl fakeRenderConfig
                        |> elementToHtml
                        |> Query.has [ Selector.attribute (HtmlAttr.attribute "disabled" "true") ]
            , test "Toggle doesn't fires message on click" <|
                \_ ->
                    Button.bodyText "Click Here"
                        |> Button.toggle OnToggleChanged False
                        |> Button.withMode Button.modeDisabled
                        |> Button.toEl fakeRenderConfig
                        |> elementToHtml
                        |> findBtnQuery
                        |> Event.simulate Event.click
                        |> Event.toResult
                        |> Expect.err
            ]
        , describe "when withDisabledMode is set to False"
            [ test "Button fires OnButtonClicked message on click" <|
                \_ ->
                    Button.bodyText "Click Here"
                        |> Button.button OnButtonClicked
                        |> Button.withMode Button.modeEnabled
                        |> Button.toEl fakeRenderConfig
                        |> elementToHtml
                        |> findBtnQuery
                        |> Event.simulate Event.click
                        |> Event.expect OnButtonClicked
            , test "Toogle fires OnButtonClicked message on click" <|
                \_ ->
                    Button.bodyText "Click Here"
                        |> Button.toggle OnToggleChanged False
                        |> Button.toEl fakeRenderConfig
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
                    Button.bodyText "Click Here"
                        |> Button.toggle OnToggleChanged False
                        |> Button.toEl fakeRenderConfig
                        |> elementToHtml
                        |> findBtnQuery
                        |> Event.simulate Event.click
                        |> Event.expect (OnToggleChanged True)
            , test "Click fires the (OnToggleChanged False) when is On" <|
                \_ ->
                    Button.bodyText "Click Here"
                        |> Button.toggle OnToggleChanged True
                        |> Button.toEl fakeRenderConfig
                        |> elementToHtml
                        |> findBtnQuery
                        |> Event.simulate Event.click
                        |> Event.expect (OnToggleChanged False)
            ]
        , describe "#link"
            [ test "Click do not fires the any Message" <|
                \_ ->
                    Button.bodyText "Click Here"
                        |> Button.link (Link.link "http://www.google.com")
                        |> Button.toEl fakeRenderConfig
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
                    Button.bodyText "Click Here"
                        |> Button.link (Link.link url)
                        |> Button.toEl fakeRenderConfig
                        |> elementToHtml
                        |> Query.has [ Selector.attribute (HtmlAttr.attribute "href" url) ]
            ]
        ]


findBtnQuery : Query.Single Msg -> Query.Single Msg
findBtnQuery item =
    Query.find [ Selector.attribute (HtmlAttr.attribute "role" "button") ] item


clickableBtn : Query.Single Msg
clickableBtn =
    Button.bodyText "Click Here"
        |> Button.button OnButtonClicked
        |> Button.toEl fakeRenderConfig
        |> elementToHtml
        |> findBtnQuery
