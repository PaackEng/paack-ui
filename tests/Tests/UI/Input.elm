module Tests.UI.Input exposing (tests)

import Element
import Element.Events as Events
import Fuzz
import Html.Attributes
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Tests.Utils.Element exposing (elementToHtml, hasIconInside)
import Tests.Utils.ExtraEvents as ExtraEvents
import Tests.Utils.ExtraSelectors as ExtraSelectors
import UI.Input as Input


type Msg
    = OnInputChanged String
    | OnEnterPressed


tests =
    describe "UI.Input"
        [ labelTests
        , textTests
        , placeholderTests
        , eventsTests
        ]


labelTests : Test
labelTests =
    let
        testInput input attribute value =
            input OnInputChanged
                |> Input.withLabel value
                |> findInput
                |> Query.has [ ExtraSelectors.hasAttribute attribute value ]
    in
    describe "when withLabel called"
        [ fuzz Fuzz.string "Input.default sets the aria-label correctly" <|
            \randomString -> testInput Input.default "aria-label" randomString
        , fuzz Fuzz.string "Input.default sets the title correctly" <|
            \randomString -> testInput Input.default "title" randomString
        , fuzz Fuzz.string "Input.email sets the aria-label correctly" <|
            \randomString -> testInput Input.email "aria-label" randomString
        , fuzz Fuzz.string "Input.email sets the title correctly" <|
            \randomString -> testInput Input.email "title" randomString
        , fuzz Fuzz.string "Input.password sets the aria-label correctly" <|
            \randomString -> testInput Input.password "aria-label" randomString
        , fuzz Fuzz.string "Input.password sets the title correctly" <|
            \randomString -> testInput Input.password "title" randomString
        ]


textTests : Test
textTests =
    let
        testInput input value =
            input OnInputChanged
                |> Input.withText value
                |> findInput
                |> Query.has [ ExtraSelectors.hasAttribute "value" value ]
    in
    describe "when withText called"
        [ fuzz Fuzz.string "Input.default sets the text correctly" <|
            \randomString -> testInput Input.default randomString
        , fuzz Fuzz.string "Input.email sets the text correctly" <|
            \randomString -> testInput Input.email randomString
        , fuzz Fuzz.string "Input.password sets the text correctly" <|
            \randomString -> testInput Input.password randomString
        ]


placeholderTests : Test
placeholderTests =
    let
        testInput input value =
            input OnInputChanged
                |> Input.withText "some text"
                |> Input.withLabel "some label"
                |> Input.withPlaceholder value
                |> Input.toEl
                |> elementToHtml
                |> Query.find [ Selector.tag "label" ]
                |> Query.has [ Selector.text value ]
    in
    describe "when withPlaceholder called"
        [ fuzz Fuzz.string "Input.default sets the placeholder correctly" <|
            \randomString -> testInput Input.default randomString
        , fuzz Fuzz.string "Input.email sets the placeholder correctly" <|
            \randomString -> testInput Input.email randomString
        , fuzz Fuzz.string "Input.password sets the placeholder correctly" <|
            \randomString -> testInput Input.password randomString
        ]


eventsTests : Test
eventsTests =
    let
        defaultInput input =
            input OnInputChanged
                |> Input.withText "some text"
                |> Input.withOnEnterPressed OnEnterPressed

        testInputForOnEnterPressed input =
            defaultInput input
                |> findInput
                |> Event.simulate ExtraEvents.enterKey
                |> Event.expect OnEnterPressed

        testInputForOnInputChanged input =
            defaultInput input
                |> findInput
                |> Event.simulate (Event.input "holi")
                |> Event.expect (OnInputChanged "holi")
    in
    describe "Events"
        [ describe "when input changed event is triggered"
            [ test "Input.default triggers OnInputChanged" <|
                \_ -> testInputForOnInputChanged Input.default
            , test "Input.email triggers OnInputChanged" <|
                \_ -> testInputForOnInputChanged Input.email
            , test "Input.password triggers OnInputChanged" <|
                \_ -> testInputForOnInputChanged Input.password
            ]
        , describe "when enter is triggered"
            [ test "Input.default triggers OnEnterPressed" <|
                \_ -> testInputForOnEnterPressed Input.default
            , test "Input.email triggers OnEnterPressed" <|
                \_ -> testInputForOnEnterPressed Input.email
            , test "Input.password triggers OnEnterPressed" <|
                \_ -> testInputForOnEnterPressed Input.password
            ]
        ]


findInput : Input.Input Msg -> Query.Single Msg
findInput content =
    content
        |> Input.toEl
        |> elementToHtml
        |> Query.find [ Selector.tag "input" ]
