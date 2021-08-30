module Dropdown.Stories exposing (stories, update)

import Dropdown.Model as Dropdown
import Dropdown.Msg exposing (Msg(..))
import Element exposing (Element)
import Msg as RootMsg
import PluginOptions exposing (defaultWithMenu)
import Return exposing (Return)
import Tables.Book exposing (Book, books)
import UI.Dropdown as Dropdown exposing (Dropdown)
import UI.Effect as Effect
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UIExplorer exposing (storiesOf)
import Utils
    exposing
        ( ExplorerStory
        , ExplorerUI
        , goToDocsCallToAction
        , iconsSvgSprite
        , prettifyElmCode
        , storyWithModel
        )


update : RenderConfig -> Msg -> Dropdown.Model -> Return RootMsg.Msg Dropdown.Model
update cfg msg model =
    case msg of
        ForDropdownMsg subMsg ->
            let
                ( state, effect ) =
                    Dropdown.update cfg
                        subMsg
                        (autocompleteDropdownView model)
            in
            ( { model | dropdownState = state }
            , Effect.perform effect
            )

        SelectMsg item ->
            ( { model | selectedBook = item }, Cmd.none )

        FilterTextChanged text ->
            ( { model | filterText = text }, Cmd.none )


stories : RenderConfig -> ExplorerUI
stories cfg =
    storiesOf "Dropdown"
        [ basicDropdownStory cfg
        , filterableDropdownStory cfg
        , autocompleteDropdownStroy cfg
        ]


withIconSpreadsheet : Element msg -> Element msg
withIconSpreadsheet element =
    Element.column [ Element.width Element.fill ]
        [ iconsSvgSprite
        , element
        ]


basicDropdownStory : RenderConfig -> ExplorerStory
basicDropdownStory cfg =
    storyWithModel
        ( "Basic"
        , \{ dropdownStories } ->
            basicDropdownView dropdownStories
                |> Dropdown.renderElement cfg
                |> withIconSpreadsheet
        , { defaultWithMenu
            | code = prettifyElmCode basicDropdownCode
            , note = goToDocsCallToAction "Dropdown"
          }
        )


basicDropdownView : Dropdown.Model -> Dropdown Book RootMsg.Msg
basicDropdownView model =
    Dropdown.basic
        { dropdownMsg = ForDropdownMsg >> RootMsg.DropdownStoriesMsg
        , onSelectMsg = SelectMsg >> RootMsg.DropdownStoriesMsg
        , state = model.dropdownState
        }
        |> Dropdown.withPlaceholder "Choose a book"
        |> Dropdown.withItems books
        |> Dropdown.withSelected model.selectedBook
        |> Dropdown.withItemToText .title
        |> Dropdown.withMaximumListHeight 200


basicDropdownCode : String
basicDropdownCode =
    """
Dropdown.basic
    { dropdownMsg = ForDropdownMsg >> RootMsg.DropdownStoriesMsg
    , onSelectMsg = SelectMsg >> RootMsg.DropdownStoriesMsg
    , state = model.dropdownState
    }
    |> Dropdown.withPlaceholder "Choose a book"
    |> Dropdown.withItems books
    |> Dropdown.withSelected model.selectedBook
    |> Dropdown.withItemToText .title
    |> Dropdown.withMaximumListHeight 200
    |> Dropdown.renderElement renderConfig
    """


filterableDropdownStory : RenderConfig -> ExplorerStory
filterableDropdownStory cfg =
    storyWithModel
        ( "Filterable"
        , \{ dropdownStories } ->
            filterableDropdownView dropdownStories
                |> Dropdown.renderElement cfg
                |> withIconSpreadsheet
        , { defaultWithMenu
            | code = prettifyElmCode filterableDropdownCode
            , note = goToDocsCallToAction "Dropdown"
          }
        )


filterableDropdownView : Dropdown.Model -> Dropdown Book RootMsg.Msg
filterableDropdownView model =
    Dropdown.filterable
        { dropdownMsg = ForDropdownMsg >> RootMsg.DropdownStoriesMsg
        , onSelectMsg = SelectMsg >> RootMsg.DropdownStoriesMsg
        , state = model.dropdownState
        }
        |> Dropdown.withPlaceholder "Choose a book"
        |> Dropdown.withItems books
        |> Dropdown.withSelected model.selectedBook
        |> Dropdown.withItemToText .title


filterableDropdownCode : String
filterableDropdownCode =
    """
Dropdown.filterable
    { dropdownMsg = ForDropdownMsg >> RootMsg.DropdownStoriesMsg
    , onSelectMsg = SelectMsg >> RootMsg.DropdownStoriesMsg
    , state = model.dropdownState
    }
    |> Dropdown.withPlaceholder "Choose a book"
    |> Dropdown.withItems books
    |> Dropdown.withSelected model.selectedBook
    |> Dropdown.withItemToText .title
    |> Dropdown.renderElement renderConfig
    """


autocompleteDropdownStroy : RenderConfig -> ExplorerStory
autocompleteDropdownStroy cfg =
    storyWithModel
        ( "AutoComplete"
        , \{ dropdownStories } ->
            Element.column
                [ Element.width Element.fill
                , Element.spacing 8
                ]
                [ ("Filter text: " ++ dropdownStories.filterText)
                    |> Text.body2
                    |> Text.renderElement cfg
                , autocompleteDropdownView dropdownStories
                    |> Dropdown.renderElement cfg
                    |> withIconSpreadsheet
                ]
        , { defaultWithMenu
            | code = prettifyElmCode autocompleteDropdownCode
            , note = goToDocsCallToAction "Dropdown"
          }
        )


autocompleteDropdownView : Dropdown.Model -> Dropdown Book RootMsg.Msg
autocompleteDropdownView model =
    Dropdown.autoCompleteHelper
        { dropdownMsg = ForDropdownMsg >> RootMsg.DropdownStoriesMsg
        , onSelectMsg = SelectMsg >> RootMsg.DropdownStoriesMsg
        , onFilterChangeMsg = FilterTextChanged >> RootMsg.DropdownStoriesMsg
        , state = model.dropdownState
        }
        |> Dropdown.withPlaceholder "Choose a book"
        |> Dropdown.withItems books
        |> Dropdown.withSelected model.selectedBook
        |> Dropdown.withItemToText .title


autocompleteDropdownCode : String
autocompleteDropdownCode =
    """
Dropdown.autoCompleteHelper
    { dropdownMsg = ForDropdownMsg >> RootMsg.DropdownStoriesMsg
    , onSelectMsg = SelectMsg >> RootMsg.DropdownStoriesMsg
    , onFilterChangeMsg = FilterTextChanged >> RootMsg.DropdownStoriesMsg
    , state = model.dropdownState
    }
    |> Dropdown.withPlaceholder "Choose a book"
    |> Dropdown.withItems books
    |> Dropdown.withSelected model.selectedBook
    |> Dropdown.withItemToText .title
    |> Dropdown.renderElement renderConfig
    """
