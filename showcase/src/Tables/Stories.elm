module Tables.Stories exposing (stories, update)

import Element exposing (Element, fill)
import Element.Font exposing (italic, underline)
import Msg
import PluginOptions exposing (defaultWithMenu)
import Return as R exposing (Return)
import Tables.Model as Stories
import Tables.Msg as Stories
import Time exposing (millisToPosix)
import UI.Internal.Basics exposing (maybeNotThen)
import UI.Internal.DateInput as DateInput
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Tables.Common as Table exposing (..)
import UI.Tables.Stateful as Stateful exposing (..)
import UI.Tables.Stateless as Stateless
import UI.Text as Text exposing (Text)
import UI.TextField as TextField
import UI.Utils.TypeNumbers as T
import UIExplorer exposing (storiesOf)
import Utils exposing (iconsSvgSprite, story, storyWithModel)


update : Stories.Msg -> Stories.Model -> Return Msg.Msg Stories.Model
update msg { tableState } =
    case msg of
        Stories.ForComponent subMsg ->
            let
                ( newModel, subCmd ) =
                    Stateful.update subMsg tableState
            in
            ( { tableState = newModel }, subCmd )


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
        , { defaultWithMenu
            | note = "See [docs](https://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-Tables-Stateful) for the exact code of this example."
          }
        )


mobileTableStory =
    storyWithModel
        ( "Mobile"
        , \{ tablesStories } -> demoWithIcons mobileCfg tablesStories
        , { defaultWithMenu
            | note = "See [docs](https://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-Tables-Stateful) for the exact code of this example."
          }
        )


demoTable renderConfig model =
    let
        toTableDetails { author, title, acquired, read } =
            detailsEmpty
                |> detailHidden
                |> detailShown { label = "Author", content = cellFromText <| Text.body2 author }
                |> detailHidden
                |> detailShown
                    { label = "Acquired"
                    , content =
                        acquired
                            |> DateInput.fromPosix Time.utc
                            |> DateInput.toDD_MM_YYYY "/"
                            |> Text.body2
                            |> cellFromText
                    }
                |> detailShown
                    { label = "Read"
                    , content =
                        read
                            |> DateInput.fromPosix Time.utc
                            |> DateInput.toDD_MM_YYYY "/"
                            |> Text.body2
                            |> cellFromText
                    }

        toTableCover { title, year } =
            { title = title, caption = Just year }
    in
    Stateful.table
        { toExternalMsg = Stories.ForComponent >> Msg.TablesStoriesMsg
        , columns = tableColumns
        , toRow = toTableRow renderConfig
        , state = model.tableState
        }
        |> Stateful.withResponsive
            { toDetails = toTableDetails
            , toCover = toTableCover
            }
        |> Stateful.withWidth Element.shrink
        |> Stateful.withItems books
        |> Stateful.renderElement renderConfig


demoWithIcons renderConfig model =
    model
        |> demoTable renderConfig
        |> List.singleton
        |> (::) iconsSvgSprite
        |> Element.wrappedRow [ Element.width fill ]


mobileCfg =
    RenderConfig.init { width = 375, height = 667 } RenderConfig.localeEnglish


statelessTableStory renderConfig =
    storyWithModel
        ( "Stateless"
        , \_ -> statelessDemoTable renderConfig
        , { defaultWithMenu
            | note = "See [docs](https://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-Tables-Stateless) for the exact code of this example."
          }
        )


statelessDemoTable renderConfig =
    Stateless.table
        { columns = tableColumns
        , toRow = toTableRow renderConfig
        }
        |> Stateless.withWidth Element.shrink
        |> Stateless.withItems books
        |> Stateless.renderElement renderConfig


books =
    [ { author = "Dan Brown"
      , title = "Angels & Demons"
      , year = "2000"
      , acquired = millisToPosix 1118405730000
      , read = millisToPosix 1118405730000
      }
    , { author = "Dan Brown"
      , title = "The Da Vinci Code"
      , year = "2003"
      , acquired = millisToPosix 1183983330000
      , read = millisToPosix 1183983330000
      }
    , { author = "Dan Brown"
      , title = "The Lost Symbol"
      , year = "2009"
      , acquired = millisToPosix 1540210530000
      , read = millisToPosix 1540210530000
      }
    , { author = "Dan Brown"
      , title = "Inferno"
      , year = "2013"
      , acquired = millisToPosix 1538655330000
      , read = millisToPosix 1538655330000
      }
    , { author = "Dan Brown"
      , title = "Origin"
      , year = "2017"
      , acquired = millisToPosix 1486037730000
      , read = millisToPosix 1486037730000
      }
    , { author = "Suzanne Collins"
      , title = "The Hunger Games"
      , year = "2008"
      , acquired = millisToPosix 1230120930000
      , read = millisToPosix 1230120930000
      }
    , { author = "Agatha Christie"
      , title = "Murder on the Orient Express"
      , year = "1933"
      , acquired = millisToPosix 969711330000
      , read = millisToPosix 969711330000
      }
    ]


tableColumns =
    columnsEmpty
        |> column "Title" (columnWidthPixels 320)
        |> column "Author" (columnWidthPixels 240)
        |> column "Year" (columnWidthPixels 120)
        |> column "Acquired" (columnWidthPixels 180)
        |> column "Read" (columnWidthPixels 180)


toTableRow renderConfig { author, title, year, acquired, read } =
    let
        titleCell =
            Element.el [ underline, italic ] <| Text.renderElement renderConfig <| Text.body2 title
    in
    rowEmpty
        |> rowCellCustom titleCell
        |> rowCellText (Text.body2 author)
        |> rowCellText (Text.caption year)
        |> rowCellText (Text.caption <| DateInput.toDD_MM_YYYY "/" <| DateInput.fromPosix Time.utc acquired)
        |> rowCellText (Text.caption <| DateInput.toDD_MM_YYYY "/" <| DateInput.fromPosix Time.utc read)
