module Tests.UI.ListView exposing (tests)

import Element as Element exposing (Element)
import Html.Attributes as HtmlAttr
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Tests.Utils.Element exposing (cursorPointer, elementToHtml)
import Tests.Utils.RenderConfig exposing (desktopWindowConfig)
import UI.Icon as Icon
import UI.ListView as ListView
import UI.Utils.Action as Action


type Msg
    = FilterSet String
    | Select Whatever
    | SomeButtonClicked


tests : Test
tests =
    describe "UI.Button tests"
        [ withActionBar
        , withItems
        , withSearchField
        ]


withActionBar : Test
withActionBar =
    let
        actionBarTitle =
            "Some Title"

        component =
            ListView.selectList Select
                (.id >> String.fromInt)
                mockView
                |> ListView.withActionBar
                    { label = actionBarTitle
                    , icon = Icon.toggle
                    , action = Action.DispatchMsg SomeButtonClicked
                    }
    in
    describe "#withActionBar"
        [ test "has ActionBar" <|
            \_ ->
                component
                    |> ListView.renderElement desktopWindowConfig
                    |> elementToHtml
                    |> Query.has [ Selector.text actionBarTitle ]
        , test "the button works" <|
            \_ ->
                component
                    |> ListView.renderElement desktopWindowConfig
                    |> elementToHtml
                    |> findBtnQuery
                    |> Event.simulate Event.click
                    |> Event.expect SomeButtonClicked
        ]


withItems : Test
withItems =
    let
        lonewolf =
            Whatever 222 "Wolf"

        options =
            [ Whatever 111 "Bear", lonewolf, Whatever 333 "Fox" ]

        component =
            ListView.selectList Select
                (.id >> String.fromInt)
                mockView
                |> ListView.withItems options
    in
    describe "#withItems"
        [ test "Options are visible" <|
            \_ ->
                component
                    |> ListView.renderElement desktopWindowConfig
                    |> elementToHtml
                    |> Query.has [ Selector.text "Bear", Selector.text "Wolf", Selector.text "Fox" ]
        , test "Options are selectable" <|
            \_ ->
                component
                    |> ListView.renderElement desktopWindowConfig
                    |> elementToHtml
                    |> Query.find [ cursorPointer, Selector.containing [ Selector.text "Wolf" ] ]
                    |> Event.simulate Event.click
                    |> Event.expect (Select lonewolf)
        , test "Selected is correctly applied" <|
            \_ ->
                component
                    |> ListView.withSelected (\{ id } -> id == 222)
                    |> ListView.renderElement desktopWindowConfig
                    |> elementToHtml
                    |> Query.has [ cursorPointer, Selector.containing [ Selector.text "Wolf", Selector.text selectedPseudoTag ] ]
        ]


withSearchField : Test
withSearchField =
    let
        searchLabel =
            "Searching for..."

        component filterState =
            ListView.selectList Select
                (.id >> String.fromInt)
                mockView
                |> ListView.withSearchField
                    { title = "Cats"
                    , label = searchLabel
                    , searchMsg = FilterSet
                    , currentFilter = filterState
                    }

        theField =
            [ Selector.tag "input"
            , Selector.attribute (HtmlAttr.attribute "title" searchLabel)
            ]

        exampleDog =
            "Mountain Dog"

        exampleOpt =
            [ Whatever 42 "Cat", Whatever 23 exampleDog ]

        filter target { label } =
            String.contains target label
    in
    describe "#withSearchField"
        [ test "Field has label" <|
            \_ ->
                component Nothing
                    |> ListView.renderElement desktopWindowConfig
                    |> elementToHtml
                    |> Query.has theField
        , test "Field triggers message" <|
            \_ ->
                component Nothing
                    |> ListView.renderElement desktopWindowConfig
                    |> elementToHtml
                    |> Query.find theField
                    |> Event.simulate (Event.input "Dog")
                    |> Event.expect (FilterSet "Dog")
        , test "Having a filter does filter" <|
            \_ ->
                component (Just ( "Dog", filter ))
                    |> ListView.withItems exampleOpt
                    |> ListView.renderElement desktopWindowConfig
                    |> elementToHtml
                    |> Query.find [ Selector.text "OPT" ]
                    |> Query.has [ Selector.text exampleDog ]
        ]


type alias Whatever =
    { id : Int
    , label : String
    }


selectedPseudoTag : String
selectedPseudoTag =
    "SELECTED"


anyOptionTag : String
anyOptionTag =
    "OPT"


mockView : Bool -> Whatever -> Element Msg
mockView isSelected { label } =
    if isSelected then
        Element.text <| selectedPseudoTag ++ " " ++ label ++ " " ++ anyOptionTag

    else
        Element.text <| label ++ " " ++ anyOptionTag


findBtnQuery : Query.Single Msg -> Query.Single Msg
findBtnQuery item =
    Query.find [ Selector.attribute (HtmlAttr.attribute "role" "button") ] item
