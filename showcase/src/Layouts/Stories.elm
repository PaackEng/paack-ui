module Layouts.Stories exposing (stories, update)

import Element exposing (Element, fill, maximum, px)
import Layouts.Model as LayoutsModel
import Layouts.Msg as LayoutsMsg
import Model exposing (Model)
import Msg exposing (Msg)
import PluginOptions exposing (defaultWithoutMenu)
import Return exposing (Return)
import Tables.Book exposing (Book, books)
import UI.Badge as Badge
import UI.Internal.NavigationContainer
import UI.Layout.SplitSelectable as SplitSelectable
import UI.ListView as ListView exposing (ListView)
import UI.ListView.SummaryItem as Summary
import UI.Palette as Palette exposing (brightnessLighter, tonePrimary)
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerStory, ExplorerUI, iconsSvgSprite, prettifyElmCode, storyBorder, storyWithModel)


update : LayoutsMsg.Msg -> LayoutsModel.Model -> Return LayoutsMsg.Msg LayoutsModel.Model
update msg model =
    case msg of
        LayoutsMsg.Select book ->
            ( { model | selected = Just book }, Cmd.none )

        LayoutsMsg.Filter "" ->
            ( { model | filter = Nothing }, Cmd.none )

        LayoutsMsg.Filter text ->
            ( { model | filter = Just text }, Cmd.none )

        LayoutsMsg.ToggleExtraMenu ->
            ( { model | showExtraMenu = not <| model.showExtraMenu }, Cmd.none )


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
            >> storyBorder
            >> List.singleton
            >> (::) iconsSvgSprite
            >> Element.column [ Element.width fill ]
        , { defaultWithoutMenu | code = code }
        )


view : RenderConfig -> Model -> Element Msg
view renderConfig { layoutsStories } =
    SplitSelectable.desktop renderConfig
        { getKey = .isbn
        , items = books
        , listView = listView renderConfig layoutsStories
        , selected = Maybe.map .isbn layoutsStories.selected
        , selectedView = selectedView renderConfig layoutsStories
        }
        |> UI.Internal.NavigationContainer.toShowcaseElement
        |> Element.el [ Element.height (px 450) ]


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


listView : RenderConfig -> LayoutsModel.Model -> ListView Book Msg
listView renderConfig model =
    listItemView renderConfig
        |> ListView.selectList
            (Msg.LayoutsStoriesMsg << LayoutsMsg.Select)
            .isbn
        |> ListView.withSearchField (searchField model)
        |> ListView.withBadgedHeader "Books"
            (Badge.primaryLight "NEW")
        |> ListView.withCustomExtraMenu (Msg.LayoutsStoriesMsg LayoutsMsg.ToggleExtraMenu)
            model.showExtraMenu
            ("Hello" |> Badge.grayLight |> Badge.renderElement renderConfig)


searchField : LayoutsModel.Model -> ListView.SearchConfig Book Msg
searchField model =
    { label = "Search"
    , searchMsg = Msg.LayoutsStoriesMsg << LayoutsMsg.Filter
    , currentFilter = bookFilter model.filter
    }


bookFilter : Maybe String -> Maybe ( String, String -> Book -> Bool )
bookFilter =
    Maybe.map (\str -> ( str, bookHasString ))


bookHasString : String -> Book -> Bool
bookHasString str { title } =
    String.contains (String.toLower str) (String.toLower title)


listItemView : RenderConfig -> Bool -> Book -> Element Msg
listItemView renderConfig isSelected book =
    Summary.summaryItem book.title book.author
        |> Summary.withBadge (Badge.grayLight <| String.fromInt 1)
        |> Summary.withSelected isSelected
        |> Summary.renderElement renderConfig


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
