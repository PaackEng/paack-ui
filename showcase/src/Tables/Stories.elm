module Tables.Stories exposing (stories, update)

import Element exposing (Element, fill)
import Msg
import PluginOptions exposing (defaultWithMenu)
import Return as R exposing (Return)
import Tables.Book as Book
import Tables.Model as Stories
import Tables.Msg as Stories
import Time
import UI.Internal.Basics exposing (maybeNotThen)
import UI.Internal.DateInput as DateInput
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Tables.Stateful as Stateful
import UI.Tables.Stateless as Stateless
import UI.Text as Text exposing (Text)
import UI.TextField as TextField
import UI.Utils.TypeNumbers as T
import UIExplorer exposing (storiesOf)
import Utils exposing (iconsSvgSprite, story, storyWithModel)


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


stories renderConfig =
    storiesOf
        "Tables"
        [ desktopTableStory renderConfig
        , mobileTableStory renderConfig
        , statelessTableStory renderConfig
        , selectableTableStory renderConfig
        ]


desktopTableStory renderConfig =
    storyWithModel
        ( "Desktop"
        , \{ tablesStories } -> withIcons renderConfig (demoTable renderConfig tablesStories.mainTableState Stories.ForMain)
        , { defaultWithMenu
            | note = "See [docs](https://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-Tables-Stateful) for the exact code of this example."
          }
        )


mobileTableStory renderConfig =
    storyWithModel
        ( "Mobile"
        , \{ tablesStories } -> withIcons mobileCfg (demoTable renderConfig tablesStories.mainTableState Stories.ForMain)
        , { defaultWithMenu
            | note = "See [docs](https://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-Tables-Stateful) for the exact code of this example."
          }
        )


demoTable renderConfig state msg =
    Stateful.table
        { toExternalMsg = msg >> Msg.TablesStoriesMsg
        , columns = Book.tableColumns
        , toRow = Book.toTableRow renderConfig
        , state = state
        }
        |> Stateful.withResponsive
            { toDetails = Book.toTableDetails
            , toCover = Book.toTableCover
            }
        |> Stateful.withWidth Element.shrink
        |> Stateful.withItems Book.books


withIcons renderConfig table =
    table
        |> Stateful.renderElement renderConfig
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
        { columns = Book.tableColumns
        , toRow = Book.toTableRow renderConfig
        }
        |> Stateless.withWidth Element.shrink
        |> Stateless.withItems Book.books
        |> Stateless.renderElement renderConfig


selectableTableStory renderConfig =
    storyWithModel
        ( "Selectable"
        , \{ tablesStories } -> withIcons renderConfig (demoTable renderConfig tablesStories.selecTableState Stories.ForSelectable)
        , { defaultWithMenu
            | note = "See [docs](https://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-Tables-Stateful) for the exact code of this example."
          }
        )
