module Tables.Stories exposing (stories, update)

import Element exposing (Element, fill)
import Msg
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
        , \{ tablesStories } -> demoWithIcons renderConfig tablesStories
        , { note = "See [docs](https://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-Table) for the exact code of this example." }
        )


mobileTableStory =
    storyWithModel
        ( "Mobile"
        , \{ tablesStories } -> demoWithIcons mobileCfg tablesStories
        , { note = "See [docs](https://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-Table) for the exact code of this example." }
        )


demoTable renderConfig model =
    let
        tableColumns =
            columnsEmpty
                |> columnsPush (headerToColumn "Title" |> columnWidthPixels 320)
                |> columnsPush (headerToColumn "Author" |> columnWidthPixels 240)
                |> columnsPush (headerToColumn "Year" |> columnWidthPixels 120)

        toTableRow { author, title, year } =
            rowEmpty
                |> rowPushText (Text.body1 title)
                |> rowPushText (Text.body2 author)
                |> rowPushText (Text.caption year)

        toTableDetails { author, title } =
            detailsEmpty
                |> detailsPushHidden
                |> detailsPush { label = "Author", content = cellFromText <| Text.body2 author }
                |> detailsPushHidden

        toTableCover { title, year } =
            { title = title, caption = Just year }

        someFilters =
            filtersEmpty
                |> filtersPushSingleText Nothing (filterLocal (\{ title } str -> String.contains str title))
                |> filtersPushSingleText (Just "Dan") (filterLocal (\{ author } str -> String.contains str author))
                |> filtersPushSingleText Nothing (filterLocal (\{ year } str -> String.contains str year))
    in
    Table.table (Stories.ForComponent >> Msg.TablesStoriesMsg)
        tableColumns
        toTableRow
        |> Table.withResponsive
            { toDetails = toTableDetails
            , toCover = toTableCover
            }
        |> Table.withState model.tableState
        |> Table.withWidth Element.shrink
        |> Table.withFilters someFilters
        |> Table.withItems
            [ { author = "Dan Brown", title = "Angels & Demons", year = "2000" }
            , { author = "Dan Brown", title = "The Da Vinci Code", year = "2003" }
            , { author = "Dan Brown", title = "The Lost Symbol", year = "2009" }
            , { author = "Dan Brown", title = "Inferno", year = "2013" }
            , { author = "Dan Brown", title = "Origin", year = "2017" }
            , { author = "Suzanne Collins", title = "The Hunger Games", year = "2008" }
            , { author = "Agatha Christie", title = "Murder on the Orient Express", year = "1933" }
            ]
        |> Table.renderElement renderConfig


demoWithIcons renderConfig model =
    model
        |> demoTable renderConfig
        |> List.singleton
        |> (::) iconsSvgSprite
        |> Element.wrappedRow [ Element.width fill ]


mobileCfg =
    RenderConfig.fromWindow { width = 375, height = 667 }
