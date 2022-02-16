module Tables.Stories exposing (stories, update)

import Element exposing (Element, fill, px)
import Msg exposing (Msg)
import Return exposing (Return)
import Tables.Book as Book exposing (Book)
import Tables.Model as Stories
import Tables.Msg as Stories
import UI.Effects as Effect
import UI.RenderConfig exposing (RenderConfig)
import UI.Tables.Common as Tables
import UI.Tables.Stateful as Stateful
import UI.Tables.Stateless as Stateless
import UI.Utils.TypeNumbers as T
import UIExplorer exposing (storiesOf)
import Utils
    exposing
        ( ExplorerStory
        , ExplorerUI
        , iconsSvgSprite
        , mobileRenderConfig
        , reducedToDocs
        , storyWithModel
        )


update : Stories.Msg -> Stories.Model -> Return Msg.Msg Stories.Model
update msg ({ mainTableState, selecTableState } as model) =
    case msg of
        Stories.ForMain subMsg ->
            mainTableState
                |> Stateful.update subMsg
                |> Tuple.mapFirst (\s -> { model | mainTableState = s })
                |> Tuple.mapSecond Effect.perform

        Stories.ForSelectable subMsg ->
            selecTableState
                |> Stateful.update subMsg
                |> Tuple.mapFirst (\s -> { model | selecTableState = s })
                |> Tuple.mapSecond Effect.perform


stories : RenderConfig -> ExplorerUI
stories renderConfig =
    storiesOf
        "Tables"
        [ desktopTableStory renderConfig
        , mobileTableStory
        , statelessTableStory renderConfig
        , selectableTableStory renderConfig
        , portionColumnsTable renderConfig
        ]


desktopTableStory : RenderConfig -> ExplorerStory
desktopTableStory renderConfig =
    storyWithModel
        ( "Desktop"
        , \{ tablesStories } ->
            demoTable renderConfig
                Book.tablePixelColumns
                tablesStories.mainTableState
                Stories.ForMain
                |> withIcons
        , reducedToDocs "Tables-Stateful"
        )


mobileTableStory : ExplorerStory
mobileTableStory =
    storyWithModel
        ( "Mobile"
        , \{ tablesStories } ->
            demoTable mobileRenderConfig
                Book.tablePixelColumns
                tablesStories.mainTableState
                Stories.ForMain
                |> Element.el
                    [ Element.width fill
                    , Element.height (px 450)
                    ]
                |> withIcons
        , reducedToDocs "Tables-Stateful"
        )


demoTable :
    RenderConfig
    -> Tables.Columns T.Five
    -> Stateful.State Msg Book T.Five
    -> (Stateful.Msg Book -> Stories.Msg)
    -> Element Msg
demoTable renderConfig columns state msg =
    Stateful.table
        { toExternalMsg = msg >> Msg.TablesStoriesMsg
        , columns = columns
        , toRow = Book.toTableRow
        , state = state
        }
        |> Stateful.withResponsive
            { toDetails = Book.toTableDetails
            , toCover = Book.toTableCover
            }
        |> Stateful.withWidth Element.shrink
        |> Stateful.withHeight (Element.px 480)
        |> Stateful.renderElement renderConfig


withIcons : Element Msg -> Element Msg
withIcons table =
    table
        |> List.singleton
        |> (::) iconsSvgSprite
        |> Element.wrappedRow [ Element.width fill ]


statelessTableStory : RenderConfig -> ExplorerStory
statelessTableStory renderConfig =
    storyWithModel
        ( "Stateless"
        , \_ -> statelessDemoTable renderConfig
        , reducedToDocs "Tables-Stateless"
        )


statelessDemoTable : RenderConfig -> Element Msg
statelessDemoTable renderConfig =
    Stateless.table
        { columns = Book.tablePixelColumns
        , toRow = Book.toTableRow
        }
        |> Stateless.withWidth Element.shrink
        |> Stateless.withItems Book.books
        |> Stateless.renderElement renderConfig


selectableTableStory : RenderConfig -> ExplorerStory
selectableTableStory renderConfig =
    storyWithModel
        ( "Selectable"
        , \{ tablesStories } ->
            demoTable renderConfig
                Book.tablePixelColumns
                tablesStories.selecTableState
                Stories.ForSelectable
                |> withIcons
        , reducedToDocs "Tables-Stateful"
        )


portionColumnsTable : RenderConfig -> ExplorerStory
portionColumnsTable renderConfig =
    storyWithModel
        ( "Portion Columns"
        , \{ tablesStories } ->
            demoTable renderConfig
                Book.tablePortionColumns
                tablesStories.selecTableState
                Stories.ForSelectable
                |> withIcons
        , reducedToDocs "Tables-Stateful"
        )
