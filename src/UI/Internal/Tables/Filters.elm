module UI.Internal.Tables.Filters exposing (..)

import UI.Effect as Effect exposing (Effect)
import UI.Internal.Basics exposing (flip)
import UI.Internal.Filter.Model exposing (Filter, filterGet)
import UI.Internal.Filter.Msg as Filter
import UI.Internal.Filter.Update as Filter
import UI.Internal.NArray as NArray exposing (NArray)
import UI.Utils.TypeNumbers as T


type Msg
    = FilterMsg Int Filter.Msg


type alias Filters msg item columns =
    NArray (Filter msg item) columns


itemsApplyFilters : Filters msg item columns -> List item -> List item
itemsApplyFilters filters items =
    filters
        |> NArray.toList
        |> List.filterMap filterGet
        |> List.foldl filtersReduce (always True)
        |> flip List.filter items



-- Filters


empty : Filters msg item T.Zero
empty =
    NArray.empty


get : Int -> Filters msg item columns -> Maybe (Filter msg item)
get column model =
    NArray.get column model


set : Int -> Filter msg item -> Filters msg item columns -> Filters msg item columns
set column newValue model =
    NArray.set column newValue model


push : Filter msg item -> Filters msg item columns -> Filters msg item (T.Increase columns)
push newValue model =
    NArray.push newValue model



-- Update


update : Msg -> Filters msg item columns -> ( Filters msg item columns, Filter.OutMsg msg )
update (FilterMsg column subMsg) model =
    case get column model of
        Just filter ->
            let
                ( newFilter, outMsg ) =
                    Filter.update subMsg filter
            in
            ( set column newFilter model, outMsg )

        Nothing ->
            ( model, Filter.emptyOutMsg )



-- Filters combination


filtersReduce : (item -> Bool) -> (item -> Bool) -> (item -> Bool)
filtersReduce new old =
    \item -> new item && old item
