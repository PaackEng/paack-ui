module Layouts.Stories exposing (stories, update)

import Element exposing (Element)
import Layouts.Model as LayoutsModel
import Layouts.Msg as LayoutsMsg
import Model exposing (Model)
import Msg exposing (Msg)
import PluginOptions exposing (defaultWithoutMenu)
import Return exposing (Return)
import Tables.Book exposing (Book, books)
import UI.Button as Button
import UI.Layout.SplitSelectable as SplitSelectable
import UI.ListView as ListView exposing (ListView)
import UI.NavigationContainer as Nav
import UI.RenderConfig exposing (RenderConfig)
import UI.SummaryListItem as Summary
import UI.TextField as TextField
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, goToDocsCallToAction, prettifyElmCode, storyWithModel)


update : LayoutsMsg.Msg -> LayoutsModel.Model -> Return LayoutsMsg.Msg LayoutsModel.Model
update msg model =
    case msg of
        LayoutsMsg.NoOp ->
            ( model, Cmd.none )


stories : RenderConfig -> ExplorerUI
stories cfg =
    storiesOf
        "Layouts"
        [ demo cfg
        ]


demo : RenderConfig -> ExplorerStory
demo renderConfig =
    storyWithModel
        ( "SplitSelectable"
        , view renderConfig
        , { defaultWithoutMenu | code = code }
        )


view : RenderConfig -> Model -> Element Msg
view renderConfig { layoutsStories } =
    let
        msg =
            Msg.LayoutsStoriesMsg
    in
    SplitSelectable.desktop renderConfig
        { getKey = .isbn
        , items = books
        , listView = listView renderConfig layoutsStories
        , selected = Nothing
        , selectedView = Element.none
        }
        |> Nav.toShowcaseElement


listView : RenderConfig -> LayoutsModel.Model -> ListView Book Msg
listView renderConfig model =
    listItemView renderConfig
        |> ListView.selectList (always (Msg.LayoutsStoriesMsg LayoutsMsg.NoOp))


listItemView : RenderConfig -> Bool -> Book -> Element Msg
listItemView renderConfig isSelected book =
    Summary.view renderConfig
        isSelected
        book.title
        book.author
        0


code : String
code =
    prettifyElmCode """
SplitSelectable.desktop renderConfig  
    { getKey = .isbn
    , items = books
    , listView = listView renderConfig layoutsStories
    , selected = Nothing
    , selectedView = Element.none
    }
"""
