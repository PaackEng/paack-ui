module Tables.Stories exposing (stories, update)

import Element exposing (Element, fill)
import Msg exposing (Msg)
import Return exposing (Return)
import Tables.Book as Book exposing (Book)
import Tables.Model as Stories
import Tables.Msg as Stories
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Tables.Common as Tables
import UI.Tables.Stateful as Stateful exposing (StatefulTable)
import UI.Tables.Stateless as Stateless
import UI.Utils.TypeNumbers as T
import UIExplorer exposing (storiesOf)
import Utils
    exposing
        ( ExplorerStory
        , ExplorerUI
        , iconsSvgSprite
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

        Stories.ForSelectable subMsg ->
            selecTableState
                |> Stateful.update subMsg
                |> Tuple.mapFirst (\s -> { model | selecTableState = s })


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
            demoTable
                Book.tablePixelColumns
                tablesStories.mainTableState
                Stories.ForMain
                |> withIcons renderConfig
        , reducedToDocs "Tables-Stateful"
        )


mobileTableStory : ExplorerStory
mobileTableStory =
    storyWithModel
        ( "Mobile"
        , \{ tablesStories } ->
            demoTable
                Book.tablePixelColumns
                tablesStories.mainTableState
                Stories.ForMain
                |> withIcons mobileCfg
        , reducedToDocs "Tables-Stateful"
        )


demoTable :
    Tables.Columns T.Five
    -> Stateful.State Msg Book T.Five
    -> (Stateful.Msg Book -> Stories.Msg)
    -> StatefulTable Msg Book T.Five
demoTable columns state msg =
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
        |> Stateful.withItems Book.books


withIcons : RenderConfig -> StatefulTable Msg Book T.Five -> Element Msg
withIcons renderConfig table =
    table
        |> Stateful.renderElement renderConfig
        |> List.singleton
        |> (::) iconsSvgSprite
        |> Element.wrappedRow [ Element.width fill ]


mobileCfg : RenderConfig
mobileCfg =
    RenderConfig.init
        { width = 375, height = 667 }
        RenderConfig.localeEnglish


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
            withIcons
                renderConfig
                (demoTable
                    Book.tablePixelColumns
                    tablesStories.selecTableState
                    Stories.ForSelectable
                )
        , reducedToDocs "Tables-Stateful"
        )


portionColumnsTable : RenderConfig -> ExplorerStory
portionColumnsTable renderConfig =
    storyWithModel
        ( "Portion Columns"
        , \{ tablesStories } ->
            demoTable
                Book.tablePortionColumns
                tablesStories.selecTableState
                Stories.ForSelectable
                |> withIcons renderConfig
        , reducedToDocs "Tables-Stateful"
        )
