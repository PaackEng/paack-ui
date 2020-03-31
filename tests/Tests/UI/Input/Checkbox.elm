module Tests.UI.Input.Checkbox exposing (tests)

import Expect
import Fuzz
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Tests.Utils.Element exposing (elementToHtml, hasIconInside)
import Tests.Utils.ExtraSelectors as ExtraSelectors
import UI.Input.Checkbox as Checkbox


type Msg
    = OnCheckboxChanged Bool


tests =
    describe "UI.Input.Checkbox"
        [ labelTests
        , isCheckedTests
        , disabledTests
        , eventsTests
        ]


labelTests : Test
labelTests =
    describe "when withLabel called"
        [ fuzz Fuzz.string "sets the aria-label correctly" <|
            \randomString ->
                Checkbox.checkbox OnCheckboxChanged
                    |> Checkbox.withLabel randomString
                    |> findCheckbox
                    |> Query.has [ ExtraSelectors.hasAttribute "aria-label" randomString ]
        , fuzz Fuzz.string "sets the title correctly" <|
            \randomString ->
                Checkbox.checkbox OnCheckboxChanged
                    |> Checkbox.withLabel randomString
                    |> findCheckbox
                    |> Query.has [ ExtraSelectors.hasAttribute "aria-label" randomString ]
        ]


isCheckedTests : Test
isCheckedTests =
    describe "when the checked value is set"
        [ describe "when is checked receives True"
            [ test "sets the aria-checked to True" <|
                \_ ->
                    Checkbox.checkbox OnCheckboxChanged
                        |> Checkbox.withIsChecked True
                        |> findCheckbox
                        |> Query.has [ ExtraSelectors.hasAttribute "aria-checked" "true" ]
            ]
        , describe "when is checked receives False"
            [ test "sets the aria-checked to false" <|
                \_ ->
                    Checkbox.checkbox OnCheckboxChanged
                        |> Checkbox.withIsChecked False
                        |> findCheckbox
                        |> Query.has [ ExtraSelectors.hasAttribute "aria-checked" "false" ]
            ]
        ]


disabledTests : Test
disabledTests =
    describe "when the disabled value is set"
        [ describe "when disabled receives True"
            [ test "sets aria-disabled to true" <|
                \_ ->
                    Checkbox.checkbox OnCheckboxChanged
                        |> Checkbox.withDisabledMode True
                        |> findCheckbox
                        |> Query.has [ ExtraSelectors.hasAttribute "aria-disabled" "true" ]
            ]
        , describe "when disabled receives False"
            [ test "sets aria-disabled to false" <|
                \_ ->
                    Checkbox.checkbox OnCheckboxChanged
                        |> Checkbox.withDisabledMode False
                        |> findCheckbox
                        |> Query.has [ ExtraSelectors.hasAttribute "aria-disabled" "false" ]
            ]
        ]


eventsTests : Test
eventsTests =
    describe "When user clicks on checkbox"
        [ describe "when the checkbox is checked"
            [ test "fires the message with False as the Bool value" <|
                \_ ->
                    Checkbox.checkbox OnCheckboxChanged
                        |> Checkbox.withIsChecked False
                        |> findCheckbox
                        |> Event.simulate Event.click
                        |> Event.expect (OnCheckboxChanged True)
            ]
        , describe "when the checkbox is not checked"
            [ test "fires the message with True as the Bool value" <|
                \_ ->
                    Checkbox.checkbox OnCheckboxChanged
                        |> Checkbox.withIsChecked False
                        |> findCheckbox
                        |> Event.simulate Event.click
                        |> Event.expect (OnCheckboxChanged True)
            ]
        , describe "when the checkbox is disabled"
            [ test "don't fire any message" <|
                \_ ->
                    Checkbox.checkbox OnCheckboxChanged
                        |> Checkbox.withIsChecked True
                        |> Checkbox.withDisabledMode True
                        |> findCheckbox
                        |> Event.simulate Event.click
                        |> Event.toResult
                        |> Expect.notEqual (Ok (OnCheckboxChanged False))
            ]
        ]


findCheckbox : Checkbox.Checkbox Msg -> Query.Single Msg
findCheckbox content =
    content
        |> Checkbox.toEl
        |> elementToHtml
        |> Query.find [ ExtraSelectors.checkbox ]
