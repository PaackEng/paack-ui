module UI.Internal.Tables.Common exposing (..)

import Element exposing (Element)
import UI.Button exposing (Button)
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
    | Custom (Element msg)
