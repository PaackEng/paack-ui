module Tables.Model exposing (..)

import UI.Table as Table


type alias Model =
    { tableState : Table.State }


initModel : Model
initModel =
    { tableState = Table.stateInit }
