module UI.Internal.Tables.Sorters exposing
    ( ColumnStatus
    , Msg(..)
    , Sorters
    , get
    , itemsApplySorting
    , sortAscending
    , sortBy
    , sortDescending
    , sortersEmpty
    , unsortable
    , update
    )

import UI.Effect as Effect exposing (Effect)
import UI.Internal.Analytics as Analytics
import UI.Internal.Filter.Sorter as Sorter exposing (Sorter(..), SortingDirection(..))
import UI.Internal.NArray as NArray exposing (NArray)
import UI.Utils.TypeNumbers as T


type Msg
    = SetSorting Int SortingDirection
    | ClearSorting


type alias SortingStatus item =
    { column : Int, by : Sorter item, direction : SortingDirection }


type Sorters item columns
    = Sorters
        { columns : NArray (Maybe (Sorter item)) columns
        , status : Maybe (SortingStatus item)
        }


type alias ColumnStatus =
    Maybe (Maybe SortingDirection)


update : Msg -> Sorters item columns -> ( Sorters item columns, Effect msg )
update msg sorters =
    case msg of
        SetSorting index direction ->
            let
                reverse =
                    direction == SortDescending
            in
            ( sort direction index sorters
            , Analytics.SetSorting index reverse
                |> Analytics.TableAnalytics
                |> Effect.analytics
            )

        ClearSorting ->
            ( clear sorters
            , Analytics.ClearSorting
                |> Analytics.TableAnalytics
                |> Effect.analytics
            )


clear : Sorters item columns -> Sorters item columns
clear (Sorters sorters) =
    Sorters { sorters | status = Nothing }


itemsApplySorting : Sorters item columns -> List item -> List item
itemsApplySorting (Sorters sorters) items =
    case sorters.status of
        Nothing ->
            items

        Just status ->
            let
                sortedItems =
                    Sorter.sort status.by items
            in
            case status.direction of
                SortAscending ->
                    sortedItems

                SortDescending ->
                    List.reverse sortedItems


sortersEmpty : Sorters item T.Zero
sortersEmpty =
    Sorters
        { columns = NArray.empty
        , status = Nothing
        }


sortBy :
    (item -> String)
    -> Sorters item columns
    -> Sorters item (T.Increase columns)
sortBy fn (Sorters accu) =
    Sorters
        { columns = NArray.push (Just <| AlphabeticalSortable fn) accu.columns
        , status = accu.status
        }


unsortable : Sorters item columns -> Sorters item (T.Increase columns)
unsortable (Sorters accu) =
    Sorters
        { columns = NArray.push Nothing accu.columns
        , status = accu.status
        }


sortAscending : Int -> Sorters item columns -> Sorters item columns
sortAscending =
    sort SortAscending


sortDescending : Int -> Sorters item columns -> Sorters item columns
sortDescending =
    sort SortDescending


sort : SortingDirection -> Int -> Sorters item columns -> Sorters item columns
sort direction column ((Sorters accu) as sorters) =
    case Maybe.andThen identity <| NArray.get column accu.columns of
        Just by ->
            Sorters
                { accu
                    | status =
                        Just
                            { column = column
                            , by = by
                            , direction = direction
                            }
                }

        Nothing ->
            sorters


get : Int -> Sorters item columns -> ColumnStatus
get index (Sorters { columns, status }) =
    let
        getDirection currentSorting =
            if currentSorting.column == index then
                Just currentSorting.direction

            else
                Nothing

        isSortableThen sortable =
            case sortable of
                Just _ ->
                    Just (Maybe.andThen getDirection status)

                Nothing ->
                    Nothing
    in
    Maybe.andThen
        isSortableThen
        (NArray.get index columns)
