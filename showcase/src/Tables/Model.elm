module Tables.Model exposing (..)

import UI.Tables.Stateful as Table


type alias Model =
    { tableState : Table.State }


initModel : Model
initModel =
    { tableState = Table.init }
