module Layouts.Stories exposing (stories, update)

import Element exposing (Element, fill, maximum)
import Layouts.Model as LayoutsModel
import Layouts.Msg as LayoutsMsg
import Model exposing (Model)
import Msg exposing (Msg)
import PluginOptions exposing (defaultWithoutMenu)
import Return exposing (Return)
import Tables.Book exposing (Book, books)
import UI.Layout.SplitSelectable as SplitSelectable
import UI.ListView as ListView exposing (ListView)
import UI.NavigationContainer as Nav
import UI.Internal.NavigationContainer
import UI.Palette as Palette exposing (brightnessLighter, tonePrimary)
import UI.RenderConfig exposing (RenderConfig)
import UI.SummaryListItem as Summary
import UI.Text as Text
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, prettifyElmCode, storyWithModel)


update : LayoutsMsg.Msg -> LayoutsModel.Model -> Return LayoutsMsg.Msg LayoutsModel.Model
update msg model =
    case msg of
        LayoutsMsg.NoOp ->
            ( model, Cmd.none )

        LayoutsMsg.Select book ->
            ( { model | selected = Just book }, Cmd.none )


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
    SplitSelectable.desktop renderConfig
        { getKey = .isbn
        , items = books
        , listView = listView renderConfig
        , selected = Maybe.map .isbn layoutsStories.selected
        , selectedView = selectedView renderConfig layoutsStories
        }
        |> UI.Internal.NavigationContainer.toShowcaseElement


selectedView : RenderConfig -> LayoutsModel.Model -> Element msg
selectedView renderConfig model =
    case model.selected of
        Just book ->
            Element.column
                [ Element.spacing 24
                , Element.padding 32
                , Element.width (fill |> maximum 600)
                ]
                [ Element.column [ Element.spacing 16 ]
                    [ book.title
                        |> Text.heading4
                        |> Text.renderElement renderConfig
                    , "By "
                        ++ book.author
                        |> Text.subtitle1
                        |> Text.withColor (Palette.color tonePrimary brightnessLighter)
                        |> Text.renderElement renderConfig
                    ]
                , Element.column
                    [ Element.spacing 8 ]
                    [ "Year: "
                        ++ book.year
                        |> Text.body1
                        |> Text.renderElement renderConfig
                    , "ISBN: "
                        ++ book.isbn
                        |> Text.body1
                        |> Text.renderElement renderConfig
                    ]
                ]

        _ ->
            Element.none


listView : RenderConfig -> ListView Book Msg
listView renderConfig  =
    listItemView renderConfig
        |> ListView.selectList (Msg.LayoutsStoriesMsg << LayoutsMsg.Select)


listItemView : RenderConfig -> Bool -> Book -> Element Msg
listItemView renderConfig isSelected book =
    Summary.view renderConfig
        isSelected
        book.title
        book.author
        1


code : String
code =
    prettifyElmCode """
SplitSelectable.desktop renderConfig  
    { getKey = .isbn
    , items = books
    , listView = listView renderConfig layoutsStories
    , selected = model.selected
    , selectedView = selectedView model
    }
"""
