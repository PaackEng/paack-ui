module Tables.Model exposing (Column(..), Model, ModelFilters, getColumnFilter, initModel)


type alias Model =
    { selected : Maybe String
    , filters : ModelFilters
    , editing : Maybe ( Column, Maybe String )
    }


type alias ModelFilters =
    { columnA : Maybe String
    , columnB : Maybe String
    , columnC : Maybe String
    , columnD : Maybe String
    }


type Column
    = ColumnA
    | ColumnB
    | ColumnC
    | ColumnD


initModel : Model
initModel =
    { selected = Nothing
    , filters = ModelFilters Nothing Nothing Nothing Nothing
    , editing = Nothing
    }


getColumnFilter : Column -> Model -> Maybe String
getColumnFilter column { filters } =
    case column of
        ColumnA ->
            filters.columnA

        ColumnB ->
            filters.columnB

        ColumnC ->
            filters.columnC

        ColumnD ->
            filters.columnD
