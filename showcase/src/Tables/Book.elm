module Tables.Book exposing (..)

import Time exposing (millisToPosix)
import UI.Internal.DateInput as DateInput
import UI.Tables.Common as Tables exposing (cellFromText, column, columnWidthPixels, columnWidthPortion, columnsEmpty, rowCellText, rowEmpty)
import UI.Tables.Stateful as Stateful exposing (detailHidden, detailShown, detailsEmpty)
import UI.Text as Text
import UI.Utils.TypeNumbers as T


type alias Book =
    { author : String
    , title : String
    , year : String
    , acquired : Time.Posix
    , read : Time.Posix
    , isbn : String
    }


someFilters : Stateful.Filters msg Book T.Five
someFilters =
    Stateful.filtersEmpty
        |> Stateful.localMultiTextFilter [] .title
        |> Stateful.localMultiTextFilter [] .author
        |> Stateful.localSelectFilter
            [ "Last Decade", "New Millennium", "Old Century" ]
            Nothing
            (\item selected ->
                case selected of
                    0 ->
                        String.startsWith "201" item.year

                    1 ->
                        String.startsWith "20" item.year

                    2 ->
                        String.startsWith "19" item.year

                    _ ->
                        False
            )
        |> Stateful.localRangeDateFilter Time.utc Nothing Nothing .acquired
        |> Stateful.localPeriodDateFilter Time.utc Nothing Nothing .read


books : List Book
books =
    [ { author = "Dan Brown"
      , title = "Angels & Demons"
      , year = "2000"
      , acquired = millisToPosix 1118405730000
      , read = millisToPosix 1118405730000
      , isbn = "9780671027360"
      }
    , { author = "Dan Brown"
      , title = "The Da Vinci Code"
      , year = "2003"
      , acquired = millisToPosix 1183983330000
      , read = millisToPosix 1183983330000
      , isbn = "9780385504201"
      }
    , { author = "Dan Brown"
      , title = "The Lost Symbol"
      , year = "2009"
      , acquired = millisToPosix 1540210530000
      , read = millisToPosix 1540210530000
      , isbn = "9780385504225"
      }
    , { author = "Dan Brown"
      , title = "Inferno"
      , year = "2013"
      , acquired = millisToPosix 1538655330000
      , read = millisToPosix 1538655330000
      , isbn = "9789955133971"
      }
    , { author = "Dan Brown"
      , title = "Origin"
      , year = "2017"
      , acquired = millisToPosix 1486037730000
      , read = millisToPosix 1486037730000
      , isbn = "9788804681960"
      }
    , { author = "Suzanne Collins"
      , title = "The Hunger Games"
      , year = "2008"
      , acquired = millisToPosix 1230120930000
      , read = millisToPosix 1230120930000
      , isbn = "9780545229937"
      }
    , { author = "Agatha Christie"
      , title = "Murder on the Orient Express"
      , year = "1933"
      , acquired = millisToPosix 969711330000
      , read = millisToPosix 969711330000
      , isbn = "9780062693662"
      }
    , { author = "Clive Staples Lewis"
      , title = "The Chronicles of Narnia: The Lion, the Witch and The Wardrobe"
      , year = "1950"
      , acquired = millisToPosix 968701330000
      , read = millisToPosix 969210230000
      , isbn = "9780064404990"
      }
    , { author = "John Ronald Reuel Tolkien"
      , title = "Lord of The Rings, Part 3, The Return of the King"
      , year = "1955"
      , acquired = millisToPosix 964701330000
      , read = millisToPosix 965210230000
      , isbn = "9780618002245"
      }
    ]


tablePixelColumns : Tables.Columns T.Five
tablePixelColumns =
    columnsEmpty
        |> column "Title" (columnWidthPixels 320)
        |> column "Author" (columnWidthPixels 160)
        |> column "Year" (columnWidthPixels 120)
        |> column "Acquired" (columnWidthPixels 180)
        |> column "Read" (columnWidthPixels 180)


tablePortionColumns : Tables.Columns T.Five
tablePortionColumns =
    columnsEmpty
        |> column "Title" (columnWidthPortion 20)
        |> column "Author" (columnWidthPortion 10)
        |> column "Year" (columnWidthPortion 8)
        |> column "Acquired" (columnWidthPortion 12)
        |> column "Read" (columnWidthPortion 12)


toTableRow : Book -> Tables.Row msg T.Five
toTableRow { author, title, year, acquired, read } =
    rowEmpty
        |> rowCellText (Text.body2 title)
        |> rowCellText (Text.body2 author)
        |> rowCellText (Text.caption year)
        |> rowCellText (Text.caption <| DateInput.toDD_MM_YYYY "/" <| DateInput.fromPosix Time.utc acquired)
        |> rowCellText (Text.caption <| DateInput.toDD_MM_YYYY "/" <| DateInput.fromPosix Time.utc read)


toTableDetails : Book -> Stateful.Details msg T.Five
toTableDetails { author, acquired, read } =
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


toTableCover : Book -> Stateful.Cover
toTableCover { title, year } =
    { title = title, caption = Just year }
