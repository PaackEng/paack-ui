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
import UI.RenderConfig
import UI.TextField as TextField


type Msg
    = OnTextFieldChanged String
    | OnEnterPressed


fakeRenderConfig : UI.RenderConfig.RenderConfig
fakeRenderConfig =
    UI.RenderConfig.fromWindow
        { width = 1920
        , height = 1080
        }


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
            input
                |> TextField.withInput OnTextFieldChanged labelValue "some text"
                |> TextField.toEl fakeRenderConfig
                |> findTextField
                |> Query.has [ ExtraSelectors.hasAttribute attribute labelValue ]
    in
    describe "when withLabel called"
        [ fuzz Fuzz.string "TextField.forSinglelineText sets the aria-label correctly" <|
            \randomString -> testTextField TextField.forSinglelineText "aria-label" randomString
        , fuzz Fuzz.string "TextField.forSinglelineText sets the title correctly" <|
            \randomString -> testTextField TextField.forSinglelineText "title" randomString
        , fuzz Fuzz.string "TextField.forEmail sets the aria-label correctly" <|
            \randomString -> testTextField TextField.forEmail "aria-label" randomString
        , fuzz Fuzz.string "TextField.forEmail sets the title correctly" <|
            \randomString -> testTextField TextField.forEmail "title" randomString
        , fuzz Fuzz.string "TextField.forCurrentPassword sets the aria-label correctly" <|
            \randomString -> testTextField TextField.forCurrentPassword "aria-label" randomString
        , fuzz Fuzz.string "TextField.forCurrentPassword sets the title correctly" <|
            \randomString -> testTextField TextField.forCurrentPassword "title" randomString
        ]


textTests : Test
textTests =
    let
        testTextField input value =
            input
                |> TextField.withInput OnTextFieldChanged "some label" value
                |> TextField.toEl fakeRenderConfig
                |> findTextField
                |> Query.has [ ExtraSelectors.hasAttribute "value" value ]
    in
    describe "when withText called"
        [ fuzz Fuzz.string "TextField.forSinglelineText sets the text correctly" <|
            \randomString -> testTextField TextField.forSinglelineText randomString
        , fuzz Fuzz.string "TextField.forEmail sets the text correctly" <|
            \randomString -> testTextField TextField.forEmail randomString
        , fuzz Fuzz.string "TextField.forCurrentPassword sets the text correctly" <|
            \randomString -> testTextField TextField.forCurrentPassword randomString
        ]


placeholderTests : Test
placeholderTests =
    let
        testTextField input placeholderValue =
            input
                |> TextField.withInput OnTextFieldChanged "some label" "some text"
                |> TextField.withPlaceholder placeholderValue
                |> TextField.toEl fakeRenderConfig
                |> elementToHtml
                |> Query.find [ Selector.tag "label" ]
                |> Query.has [ Selector.text placeholderValue ]
    in
    describe "when withPlaceholder called"
        [ fuzz Fuzz.string "TextField.forSinglelineText sets the placeholder correctly" <|
            \randomString -> testTextField TextField.forSinglelineText randomString
        , fuzz Fuzz.string "TextField.forEmail sets the placeholder correctly" <|
            \randomString -> testTextField TextField.forEmail randomString
        , fuzz Fuzz.string "TextField.forCurrentPassword sets the placeholder correctly" <|
            \randomString -> testTextField TextField.forCurrentPassword randomString
        ]


eventsTests : Test
eventsTests =
    let
        defaultTextField input =
            input
                |> TextField.withInput OnTextFieldChanged "some label" "some text"
                |> TextField.withOnEnterPressed OnEnterPressed

        testTextFieldForOnEnterPressed input =
            defaultTextField input
                |> TextField.toEl fakeRenderConfig
                |> findTextField
                |> Event.simulate ExtraEvents.enterKey
                |> Event.expect OnEnterPressed

        testTextFieldForOnTextFieldChanged input =
            defaultTextField input
                |> TextField.toEl fakeRenderConfig
                |> findTextField
                |> Event.simulate (Event.input "holi")
                |> Event.expect (OnTextFieldChanged "holi")
    in
    describe "Events"
        [ describe "when input changed event is triggered"
            [ test "TextField.forSinglelineText triggers OnTextFieldChanged" <|
                \_ -> testTextFieldForOnTextFieldChanged TextField.forSinglelineText
            , test "TextField.forEmail triggers OnTextFieldChanged" <|
                \_ -> testTextFieldForOnTextFieldChanged TextField.forEmail
            , test "TextField.forCurrentPassword triggers OnTextFieldChanged" <|
                \_ -> testTextFieldForOnTextFieldChanged TextField.forCurrentPassword
            ]
        , describe "when enter is triggered"
            [ test "TextField.forSinglelineText triggers OnEnterPressed" <|
                \_ -> testTextFieldForOnEnterPressed TextField.forSinglelineText
            , test "TextField.forEmail triggers OnEnterPressed" <|
                \_ -> testTextFieldForOnEnterPressed TextField.forEmail
            , test "TextField.forCurrentPassword triggers OnEnterPressed" <|
                \_ -> testTextFieldForOnEnterPressed TextField.forCurrentPassword
            ]
        ]


findTextField : Element Msg -> Query.Single Msg
findTextField content =
    content
        |> elementToHtml
        |> Query.find [ Selector.tag "input" ]
