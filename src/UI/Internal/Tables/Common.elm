module UI.Internal.Tables.Common exposing (Cell(..), Column(..), ColumnOptions, ColumnWidth(..))

import Element exposing (Element)
import UI.Button exposing (Button)
import UI.Link exposing (Link)
import UI.Text exposing (Text)


type Column
    = Column String ColumnOptions


type alias ColumnOptions =
    { width : ColumnWidth
    }


type ColumnWidth
    = WidthPixels Int
    | WidthPortion Int


type Cell msg
    = CellText Text
    | CellButton (Button msg)
    | CellLink Link Text
    | CellCustom (Element msg)
