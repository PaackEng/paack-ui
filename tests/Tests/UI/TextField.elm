module Tests.UI.TextField exposing (tests)

import Element exposing (Element)
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
import Tests.Utils.RenderConfig exposing (desktopWindowConfig)
import UI.TextField as TextField


type Msg
    = OnTextFieldChanged String
    | OnEnterPressed


tests =
    describe "UI.TextField"
        [ labelTests
        , textTests
        , placeholderTests
        , eventsTests
        ]


labelTests : Test
labelTests =
    let
        testTextField input attribute labelValue =
            input OnTextFieldChanged labelValue "some text"
                |> TextField.renderElement desktopWindowConfig
                |> findTextField
                |> Query.has [ ExtraSelectors.hasAttribute attribute labelValue ]
    in
    describe "when withLabel called"
        [ fuzz Fuzz.string "TextField.singlelineText sets the aria-label correctly" <|
            \randomString -> testTextField TextField.singlelineText "aria-label" randomString
        , fuzz Fuzz.string "TextField.singlelineText sets the title correctly" <|
            \randomString -> testTextField TextField.singlelineText "title" randomString
        , fuzz Fuzz.string "TextField.email sets the aria-label correctly" <|
            \randomString -> testTextField TextField.email "aria-label" randomString
        , fuzz Fuzz.string "TextField.email sets the title correctly" <|
            \randomString -> testTextField TextField.email "title" randomString
        , fuzz Fuzz.string "TextField.currentPassword sets the aria-label correctly" <|
            \randomString -> testTextField TextField.currentPassword "aria-label" randomString
        , fuzz Fuzz.string "TextField.currentPassword sets the title correctly" <|
            \randomString -> testTextField TextField.currentPassword "title" randomString
        ]


textTests : Test
textTests =
    let
        testTextField input value =
            input OnTextFieldChanged "some label" value
                |> TextField.renderElement desktopWindowConfig
                |> findTextField
                |> Query.has [ ExtraSelectors.hasAttribute "value" value ]
    in
    describe "when withText called"
        [ fuzz Fuzz.string "TextField.singlelineText sets the text correctly" <|
            \randomString -> testTextField TextField.singlelineText randomString
        , fuzz Fuzz.string "TextField.email sets the text correctly" <|
            \randomString -> testTextField TextField.email randomString
        , fuzz Fuzz.string "TextField.currentPassword sets the text correctly" <|
            \randomString -> testTextField TextField.currentPassword randomString
        ]


placeholderTests : Test
placeholderTests =
    let
        testTextField input placeholderValue =
            input OnTextFieldChanged "some label" "some text"
                |> TextField.setLabelVisible True
                |> TextField.withPlaceholder placeholderValue
                |> TextField.renderElement desktopWindowConfig
                |> elementToHtml
                |> Query.find [ Selector.tag "label" ]
                |> Query.has [ Selector.text placeholderValue ]
    in
    describe "when withPlaceholder called"
        [ fuzz Fuzz.string "TextField.singlelineText sets the placeholder correctly" <|
            \randomString -> testTextField TextField.singlelineText randomString
        , fuzz Fuzz.string "TextField.email sets the placeholder correctly" <|
            \randomString -> testTextField TextField.email randomString
        , fuzz Fuzz.string "TextField.currentPassword sets the placeholder correctly" <|
            \randomString -> testTextField TextField.currentPassword randomString
        ]


eventsTests : Test
eventsTests =
    let
        defaultTextField input =
            input OnTextFieldChanged "some label" "some text"
                |> TextField.withOnEnterPressed OnEnterPressed

        testTextFieldForOnEnterPressed input =
            defaultTextField input
                |> TextField.renderElement desktopWindowConfig
                |> findTextField
                |> Event.simulate ExtraEvents.enterKey
                |> Event.expect OnEnterPressed

        testTextFieldForOnTextFieldChanged input =
            defaultTextField input
                |> TextField.renderElement desktopWindowConfig
                |> findTextField
                |> Event.simulate (Event.input "holi")
                |> Event.expect (OnTextFieldChanged "holi")
    in
    describe "Events"
        [ describe "when input changed event is triggered"
            [ test "TextField.singlelineText triggers OnTextFieldChanged" <|
                \_ -> testTextFieldForOnTextFieldChanged TextField.singlelineText
            , test "TextField.email triggers OnTextFieldChanged" <|
                \_ -> testTextFieldForOnTextFieldChanged TextField.email
            , test "TextField.currentPassword triggers OnTextFieldChanged" <|
                \_ -> testTextFieldForOnTextFieldChanged TextField.currentPassword
            ]
        , describe "when enter is triggered"
            [ test "TextField.singlelineText triggers OnEnterPressed" <|
                \_ -> testTextFieldForOnEnterPressed TextField.singlelineText
            , test "TextField.email triggers OnEnterPressed" <|
                \_ -> testTextFieldForOnEnterPressed TextField.email
            , test "TextField.currentPassword triggers OnEnterPressed" <|
                \_ -> testTextFieldForOnEnterPressed TextField.currentPassword
            ]
        ]


findTextField : Element Msg -> Query.Single Msg
findTextField content =
    content
        |> elementToHtml
        |> Query.find [ Selector.tag "input" ]
