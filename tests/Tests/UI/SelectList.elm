module Tests.UI.SelectList exposing (tests)

import Element as Element exposing (Element)
import Expect
import Html.Attributes as HtmlAttr
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Tests.Utils.Element exposing (cursorPointer, elementToHtml, hasIconInside)
import Tests.Utils.RenderConfig exposing (desktopWindowConfig)
import UI.Icon as Icon
import UI.Link as Link
import UI.RenderConfig exposing (RenderConfig)
import UI.SelectList as SelectList


type Msg
    = FilterSet String
    | Select Whatever
    | SomeButtonClicked


tests : Test
tests =
    describe "UI.Button tests"
        [ withActionBar
        , withOptions
        , withSearchField
        ]


withActionBar : Test
withActionBar =
    let
        actionBarTitle =
            "Some Title"

        component =
            SelectList.selectList Select mockView
                |> SelectList.withActionBar
                    actionBarTitle
                    Icon.toggle
                    SomeButtonClicked
    in
    describe "#withActionBar"
        [ test "has ActionBar" <|
            \_ ->
                component
                    |> SelectList.toEl desktopWindowConfig
                    |> elementToHtml
                    |> Query.has [ Selector.text actionBarTitle ]
        , test "the button works" <|
            \_ ->
                component
                    |> SelectList.toEl desktopWindowConfig
                    |> elementToHtml
                    |> findBtnQuery
                    |> Event.simulate Event.click
                    |> Event.expect SomeButtonClicked
        ]


withOptions : Test
withOptions =
    let
        lonewolf =
            Whatever 222 "Wolf"

        options =
            [ Whatever 111 "Bear", lonewolf, Whatever 333 "Fox" ]

        component =
            SelectList.selectList Select mockView
                |> SelectList.withOptions options
    in
    describe "#withOptions"
        [ test "Options are visible" <|
            \_ ->
                component
                    |> SelectList.toEl desktopWindowConfig
                    |> elementToHtml
                    |> Query.has [ Selector.text "Bear", Selector.text "Wolf", Selector.text "Fox" ]
        , test "Options are selectable" <|
            \_ ->
                component
                    |> SelectList.toEl desktopWindowConfig
                    |> elementToHtml
                    |> Query.find [ cursorPointer, Selector.containing [ Selector.text "Wolf" ] ]
                    |> Event.simulate Event.click
                    |> Event.expect (Select lonewolf)
        , test "Selected is correctly applied" <|
            \_ ->
                component
                    |> SelectList.withSelected (\{ id } -> id == 222)
                    |> SelectList.toEl desktopWindowConfig
                    |> elementToHtml
                    |> Query.has [ cursorPointer, Selector.containing [ Selector.text "Wolf", Selector.text selectedPseudoTag ] ]
        ]


withSearchField : Test
withSearchField =
    let
        searchLabel =
            "Searching for..."

        component filterState =
            SelectList.selectList Select mockView
                |> SelectList.withSearchField searchLabel FilterSet filterState

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
                    |> SelectList.toEl desktopWindowConfig
                    |> elementToHtml
                    |> Query.has theField
        , test "Field triggers message" <|
            \_ ->
                component Nothing
                    |> SelectList.toEl desktopWindowConfig
                    |> elementToHtml
                    |> Query.find theField
                    |> Event.simulate (Event.input "Dog")
                    |> Event.expect (FilterSet "Dog")
        , test "Having a filter does filter" <|
            \_ ->
                component (Just ( "Dog", filter ))
                    |> SelectList.withOptions exampleOpt
                    |> SelectList.toEl desktopWindowConfig
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


mockView : RenderConfig -> Bool -> Whatever -> Element Msg
mockView _ isSelected { label } =
    if isSelected then
        Element.text <| selectedPseudoTag ++ " " ++ label ++ " " ++ anyOptionTag

    else
        Element.text <| label ++ " " ++ anyOptionTag


findBtnQuery : Query.Single Msg -> Query.Single Msg
findBtnQuery item =
    Query.find [ Selector.attribute (HtmlAttr.attribute "role" "button") ] item
