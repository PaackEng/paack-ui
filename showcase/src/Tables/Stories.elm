module Tables.Stories exposing (stories, update)

import Element exposing (Element, fill)
import Msg exposing (Msg(..))
import Return as R exposing (Return)
import Tables.Model as Stories
import Tables.Msg as Stories
import UI.Internal.Basics exposing (maybeNotThen)
import UI.Internal.TypeNumbers as T
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Table as Table exposing (..)
import UI.Text as Text exposing (Text)
import UI.TextField as TextField
import UIExplorer exposing (storiesOf)
import Utils exposing (iconsSvgSprite, story, storyWithModel)


update : Stories.Msg -> Stories.Model -> Return Stories.Msg Stories.Model
update msg { tableState } =
    case msg of
        Stories.ForComponent subMsg ->
            ( { tableState = Table.stateUpdate subMsg tableState }, Cmd.none )


stories renderConfig =
    storiesOf
        "Tables"
        [ desktopTableStory renderConfig
        , mobileTableStory
        ]


desktopTableStory renderConfig =
    storyWithModel
        ( "Desktop"
        , \{ tablesStories } -> demoTable renderConfig tablesStories
        , { note = "" }
        )


mobileTableStory =
    storyWithModel
        ( "Mobile"
        , \{ tablesStories } -> demoTable mobileCfg tablesStories
        , { note = "" }
        )


demoTable renderConfig model =
    let
        tableColumns =
            columnsEmpty
                |> columnsPushHeader "Title"
                |> columnsPush (headerToColumn "Author" |> columnWidthPortion 3)

        toTableRow { author, title } =
            rowEmpty
                |> rowPushText (Text.body1 title)
                |> rowPushText (Text.body2 author)

        toTableDetails { author, title } =
            detailsEmpty
                |> detailsPushHidden
                |> detailsPush { label = "Author", content = cellFromText <| Text.body2 author }

        toTableCover { author, title } =
            { title = title, caption = Nothing }

        someFilters =
            filtersEmpty
                |> filtersPushSingleText "" (filterLocal (\{ title } str -> String.contains str title))
                |> filtersPushSingleText "" (filterLocal (\{ author } str -> String.contains str author))
    in
    Table.table (Stories.ForComponent >> Msg.TablesStoriesMsg)
        tableColumns
        toTableRow
        |> Table.withResponsive
            { toDetails = toTableDetails
            , toCover = toTableCover
            }
        |> Table.withState model.tableState
        |> Table.withWidth (Element.fill |> Element.maximum 640)
        |> Table.withFilters someFilters
        |> Table.withItems
            [ { author = "Dan Brown", title = "The Da Vinci Code" } ]
        |> Table.renderElement renderConfig
        |> List.singleton
        |> (::) iconsSvgSprite
        |> Element.wrappedRow [ Element.width fill ]


mobileCfg =
    RenderConfig.fromWindow { width = 375, height = 667 }
