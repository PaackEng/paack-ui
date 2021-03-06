module Tests.UI.Button exposing (tests)

import Expect
import Html.Attributes as HtmlAttr
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Tests.Utils.Element exposing (elementToHtml, hasIconInside)
import Tests.Utils.RenderConfig exposing (desktopWindowConfig)
import UI.Button as Button
import UI.Icon as Icon
import UI.Link as Link


type Msg
    = OnButtonClicked
    | OnToggleChanged Bool


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
                Button.fromIcon (Icon.toggle iconHintText)
                    |> Button.cmd OnButtonClicked Button.primary
                    |> Button.renderElement desktopWindowConfig
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
                    Button.fromLabel "Click Here"
                        |> Button.cmd OnButtonClicked Button.primary
                        |> Button.withDisabledIf True
                        |> Button.renderElement desktopWindowConfig
                        |> elementToHtml
                        |> findBtnQuery
                        |> Event.simulate Event.click
                        |> Event.toResult
                        |> Expect.err
            , test "Button has disabled attribute" <|
                \_ ->
                    Button.fromLabel "Click Here"
                        |> Button.disabled
                        |> Button.renderElement desktopWindowConfig
                        |> elementToHtml
                        |> Query.has [ Selector.attribute (HtmlAttr.attribute "disabled" "true") ]
            ]
        , describe "when withDisabledMode is set to False"
            [ test "Button fires OnButtonClicked message on click" <|
                \_ ->
                    Button.fromLabel "Click Here"
                        |> Button.cmd OnButtonClicked Button.danger
                        |> Button.withDisabledIf False
                        |> Button.renderElement desktopWindowConfig
                        |> elementToHtml
                        |> findBtnQuery
                        |> Event.simulate Event.click
                        |> Event.expect OnButtonClicked
            , test "Toogle fires OnButtonClicked message on click" <|
                \_ ->
                    Button.toggle "Click Here" OnToggleChanged False
                        |> Button.renderElement desktopWindowConfig
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
                    Button.toggle "Click Here" OnToggleChanged False
                        |> Button.renderElement desktopWindowConfig
                        |> elementToHtml
                        |> findBtnQuery
                        |> Event.simulate Event.click
                        |> Event.expect (OnToggleChanged True)
            , test "Click fires the (OnToggleChanged False) when is On" <|
                \_ ->
                    Button.toggle "Click Here" OnToggleChanged True
                        |> Button.renderElement desktopWindowConfig
                        |> elementToHtml
                        |> findBtnQuery
                        |> Event.simulate Event.click
                        |> Event.expect (OnToggleChanged False)
            ]
        , describe "#link"
            [ test "Click do not fires the any Message" <|
                \_ ->
                    Button.fromLabel "Click Here"
                        |> Button.redirect (Link.link "http://www.google.com") Button.hyperlink
                        |> Button.renderElement desktopWindowConfig
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
                    Button.fromLabel "Click Here"
                        |> Button.redirect (Link.link url) Button.hyperlink
                        |> Button.renderElement desktopWindowConfig
                        |> elementToHtml
                        |> Query.has [ Selector.attribute (HtmlAttr.attribute "href" url) ]
            ]
        ]


findBtnQuery : Query.Single Msg -> Query.Single Msg
findBtnQuery item =
    Query.find [ Selector.attribute (HtmlAttr.attribute "role" "button") ] item


clickableBtn : Query.Single Msg
clickableBtn =
    Button.fromLabel "Click Here"
        |> Button.cmd OnButtonClicked Button.primary
        |> Button.renderElement desktopWindowConfig
        |> elementToHtml
        |> findBtnQuery
