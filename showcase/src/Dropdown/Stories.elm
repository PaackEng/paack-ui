module Dropdown.Stories exposing (stories, update)

import Dropdown.Model as Dropdown
import Dropdown.Msg exposing (Msg(..))
import Element exposing (Element)
import Model exposing (Model)
import Msg as RootMsg
import PluginOptions exposing (PluginOptions, defaultWithMenu, defaultWithoutMenu)
import Return exposing (Return)
import Tables.Book exposing (Book, books)
import UI.Dropdown as Dropdown exposing (Dropdown)
import UI.Effect as Effect
import UI.RenderConfig exposing (RenderConfig)
import UIExplorer exposing (storiesOf)
import Utils
    exposing
        ( ExplorerStory
        , ExplorerUI
        , goToDocsCallToAction
        , iconsSvgSprite
        , prettifyElmCode
        , story
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
                        (defaultDropdownView cfg model)
            in
            ( { model | dropdownState = state }
            , Effect.perform effect
            )

        SelectMsg item ->
            ( { model | selectedBook = item }, Cmd.none )


stories : RenderConfig -> ExplorerUI
stories cfg =
    storiesOf "Dropdown"
        [ basicDropdownStory cfg
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
        ( "Default"
        , \{ dropdownStories } ->
            defaultDropdownView cfg dropdownStories
                |> Dropdown.renderElement cfg
                |> withIconSpreadsheet
        , { defaultWithoutMenu
            | code = prettifyElmCode defaultDropdownCode
            , note = goToDocsCallToAction "Dropdown"
          }
        )


defaultDropdownView : RenderConfig -> Dropdown.Model -> Dropdown Book RootMsg.Msg
defaultDropdownView renderConfig model =
    Dropdown.basic
        { dropdownMsg = ForDropdownMsg >> RootMsg.DropdownStoriesMsg
        , onSelectMsg = SelectMsg >> RootMsg.DropdownStoriesMsg
        , state = model.dropdownState
        }
        |> Dropdown.withPlaceholder "Choose a book"
        |> Dropdown.withItems books
        |> Dropdown.withSelected model.selectedBook
        |> Dropdown.withItemToText .title


defaultDropdownCode : String
defaultDropdownCode =
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
    |> Dropdown.renderElement renderConfig
    """
