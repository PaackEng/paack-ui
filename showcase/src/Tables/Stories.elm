module Tables.Stories exposing (stories, update)

import Element exposing (Element, fill)
import Msg
import Return as R exposing (Return)
import Tables.Model as Stories
import Tables.Msg as Stories
import UI.Internal.Basics exposing (maybeNotThen)
import UI.Internal.TypeNumbers as T
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Tables.Common as Table exposing (..)
import UI.Tables.Stateable as Stateable exposing (..)
import UI.Tables.Stateless as Stateless
import UI.Text as Text exposing (Text)
import UI.TextField as TextField
import UIExplorer exposing (storiesOf)
import Utils exposing (iconsSvgSprite, story, storyWithModel)


update : Stories.Msg -> Stories.Model -> Return Stories.Msg Stories.Model
update msg { tableState } =
    case msg of
        Stories.ForComponent subMsg ->
            ( { tableState = Stateable.update subMsg tableState }, Cmd.none )


stories renderConfig =
    storiesOf
        "Tables"
        [ desktopTableStory renderConfig
        , mobileTableStory
        , statelessTableStory renderConfig
        ]


desktopTableStory renderConfig =
    storyWithModel
        ( "Desktop"
        , \{ tablesStories } -> demoWithIcons renderConfig tablesStories
        , { note = "See [docs](https://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-Tables-Stateful) for the exact code of this example." }
        )


mobileTableStory =
    storyWithModel
        ( "Mobile"
        , \{ tablesStories } -> demoWithIcons mobileCfg tablesStories
        , { note = "See [docs](https://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-Tables-Stateful) for the exact code of this example." }
        )


demoTable renderConfig model =
    let
        tableColumns =
            columnsEmpty
                |> column "Title" (columnWidthPixels 320)
                |> column "Author" (columnWidthPixels 240)
                |> column "Year" (columnWidthPixels 120)

        toTableRow { author, title, year } =
            rowEmpty
                |> rowCellText (Text.body1 title)
                |> rowCellText (Text.body2 author)
                |> rowCellText (Text.caption year)

        toTableDetails { author, title } =
            detailsEmpty
                |> detailsPushHidden
                |> detailsPush { label = "Author", content = cellFromText <| Text.body2 author }
                |> detailsPushHidden

        toTableCover { title, year } =
            { title = title, caption = Just year }

        someFilters =
            filtersEmpty
                |> localSingleTextFilter Nothing .title
                |> localSingleTextFilter (Just "Dan") .author
                |> localSingleTextFilter Nothing .year
    in
    Stateable.table
        { toExtern = Stories.ForComponent >> Msg.TablesStoriesMsg
        , columns = tableColumns
        , toRow = toTableRow
        , state = model.tableState
        }
        |> Stateable.withResponsive
            { toDetails = toTableDetails
            , toCover = toTableCover
            }
        |> Stateable.withWidth Element.shrink
        |> Stateable.withFilters someFilters
        |> Stateable.withItems
            [ { author = "Dan Brown", title = "Angels & Demons", year = "2000" }
            , { author = "Dan Brown", title = "The Da Vinci Code", year = "2003" }
            , { author = "Dan Brown", title = "The Lost Symbol", year = "2009" }
            , { author = "Dan Brown", title = "Inferno", year = "2013" }
            , { author = "Dan Brown", title = "Origin", year = "2017" }
            , { author = "Suzanne Collins", title = "The Hunger Games", year = "2008" }
            , { author = "Agatha Christie", title = "Murder on the Orient Express", year = "1933" }
            ]
        |> Stateable.renderElement renderConfig


demoWithIcons renderConfig model =
    model
        |> demoTable renderConfig
        |> List.singleton
        |> (::) iconsSvgSprite
        |> Element.wrappedRow [ Element.width fill ]


mobileCfg =
    RenderConfig.fromWindow { width = 375, height = 667 }


statelessTableStory renderConfig =
    storyWithModel
        ( "Stateless"
        , \_ -> statelessDemoTable renderConfig
        , { note = "See [docs](https://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-Tables-Stateless) for the exact code of this example." }
        )


statelessDemoTable renderConfig =
    let
        tableColumns =
            columnsEmpty
                |> column "Title" (columnWidthPixels 320)
                |> column "Author" (columnWidthPixels 240)
                |> column "Year" (columnWidthPixels 120)

        toTableRow { author, title, year } =
            rowEmpty
                |> rowCellText (Text.body1 title)
                |> rowCellText (Text.body2 author)
                |> rowCellText (Text.caption year)
    in
    Stateless.table
        { columns = tableColumns
        , toRow = toTableRow
        }
        |> Stateless.withWidth Element.shrink
        |> Stateless.withItems
            [ { author = "Dan Brown", title = "Angels & Demons", year = "2000" }
            , { author = "Dan Brown", title = "The Da Vinci Code", year = "2003" }
            , { author = "Dan Brown", title = "The Lost Symbol", year = "2009" }
            , { author = "Dan Brown", title = "Inferno", year = "2013" }
            , { author = "Dan Brown", title = "Origin", year = "2017" }
            , { author = "Suzanne Collins", title = "The Hunger Games", year = "2008" }
            , { author = "Agatha Christie", title = "Murder on the Orient Express", year = "1933" }
            ]
        |> Stateless.renderElement renderConfig
