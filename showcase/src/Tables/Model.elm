module Tables.Model exposing (..)

import UI.Tables.Stateable as Table


type alias Model =
    { tableState : Table.State }


initModel : Model
initModel =
    { tableState = Table.init }
