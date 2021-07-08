module UI.Internal.Tables.Filters exposing (..)

import Array exposing (Array)
import Time exposing (Posix)
import UI.Effect as Effect exposing (Effect)
import UI.Internal.Analytics as Analytics
import UI.Internal.Basics exposing (flip, maybeNotThen)
import UI.Internal.DateInput as DateInput exposing (DateInput(..), PeriodComparison(..), PeriodDate, RangeDate)
import UI.Internal.Filter.Model exposing (..)
import UI.Internal.NArray as NArray exposing (NArray)
import UI.Utils.TypeNumbers as T


type Msg
    = EditSingleText { column : Int, value : String }
    | EditMultiText { column : Int, field : Int, value : String }
    | EditSingleDate { column : Int, value : String }
    | EditRangeFromDate { column : Int, value : String }
    | EditRangeToDate { column : Int, value : String }
    | EditPeriodDate { column : Int, value : String }
    | EditPeriodComparison { column : Int, value : PeriodComparison }
    | EditSelect { column : Int, value : Int }
    | Apply Int
    | Clear Int


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


update : Msg -> Filters msg item columns -> ( Filters msg item columns, Effect msg )
update msg model =
    case msg of
        EditSingleText { column, value } ->
            ( columnEdit singleTextEdit column value model, Effect.none )

        EditMultiText { column, field, value } ->
            ( columnEdit (multiTextEdit field) column value model, Effect.none )

        EditSingleDate { column, value } ->
            ( columnEdit singleDateEdit column value model, Effect.none )

        EditRangeFromDate { column, value } ->
            ( columnEdit rangeDateFromEdit column value model, Effect.none )

        EditRangeToDate { column, value } ->
            ( columnEdit rangeDateToEdit column value model, Effect.none )

        EditPeriodDate { column, value } ->
            ( columnEdit periodDateEdit column value model, Effect.none )

        EditPeriodComparison { column, value } ->
            ( columnEdit periodDateComparisonEdit column value model, Effect.none )

        EditSelect { column, value } ->
            ( columnEdit selectEdit column value model, Effect.none )

        Apply column ->
            applyFilter column model
                |> withApplyAnalytics column

        Clear column ->
            filterClear column model
                |> withClearAnalytics column


columnEdit :
    (value -> Filter msg item -> Filter msg item)
    -> Int
    -> value
    -> Filters msg item columns
    -> Filters msg item columns
columnEdit applier column value model =
    case get column model of
        Just filter ->
            applier value filter
                |> flip (set column) model

        Nothing ->
            model


dispatchApply : Strategy msg value item -> value -> Filters msg item columns -> ( Filters msg item columns, Effect msg )
dispatchApply strategy value newModel =
    case strategy of
        Local _ ->
            ( newModel
            , Effect.none
            )

        Remote { applyMsg } ->
            ( newModel
            , Effect.msgToCmd <| applyMsg value
            )


withApplyAnalytics : Int -> ( Filters msg item columns, Effect msg ) -> ( Filters msg item columns, Effect msg )
withApplyAnalytics column ( model, effects ) =
    let
        analytics =
            Analytics.ApplyFilter column
                |> Analytics.TableAnalytics
                |> Effect.analytics
    in
    ( model, Effect.batch [ effects, analytics ] )


dispatchClear : Strategy msg value item -> Filters msg item columns -> ( Filters msg item columns, Effect msg )
dispatchClear strategy newModel =
    case strategy of
        Local _ ->
            ( newModel
            , Effect.none
            )

        Remote { clearMsg } ->
            ( newModel
            , Effect.msgToCmd clearMsg
            )


withClearAnalytics : Int -> ( Filters msg item columns, Effect msg ) -> ( Filters msg item columns, Effect msg )
withClearAnalytics column ( model, effects ) =
    let
        analytics =
            Analytics.ClearFilter column
                |> Analytics.TableAnalytics
                |> Effect.analytics
    in
    ( model, Effect.batch [ effects, analytics ] )


applyShortcut :
    Filters msg item columns
    -> Int
    -> FilterConfig msg value item
    -> (FilterConfig msg value item -> Filter msg item)
    -> ( Filters msg item columns, Effect msg )
applyShortcut model column config constructor =
    case config.editable.current of
        Just newValue ->
            editableApply config.editable
                |> configSetEditable config
                |> constructor
                |> flip (set column) model
                |> dispatchApply config.strategy newValue

        Nothing ->
            ( model, Effect.none )


applyFilter : Int -> Filters msg item columns -> ( Filters msg item columns, Effect msg )
applyFilter column model =
    case NArray.get column model of
        Just (SingleTextFilter config) ->
            applyShortcut model column config SingleTextFilter

        Just (MultiTextFilter config) ->
            applyShortcut model column config MultiTextFilter

        Just (SingleDateFilter config) ->
            applyShortcut model column config SingleDateFilter

        Just (RangeDateFilter config) ->
            applyShortcut model column config RangeDateFilter

        Just (PeriodDateFilter config) ->
            applyShortcut model column config PeriodDateFilter

        Just (SelectFilter list config) ->
            applyShortcut model column config (SelectFilter list)

        Nothing ->
            ( model, Effect.none )


filterClear : Int -> Filters msg item columns -> ( Filters msg item columns, Effect msg )
filterClear column model =
    case NArray.get column model of
        Just (SingleTextFilter config) ->
            editableEmpty
                |> configSetEditable config
                |> SingleTextFilter
                |> flip (set column) model
                |> dispatchClear config.strategy

        Just (MultiTextFilter config) ->
            editableEmpty
                |> configSetEditable config
                |> MultiTextFilter
                |> flip (set column) model
                |> dispatchClear config.strategy

        Just (SingleDateFilter config) ->
            editableEmpty
                |> configSetEditable config
                |> SingleDateFilter
                |> flip (set column) model
                |> dispatchClear config.strategy

        Just (RangeDateFilter config) ->
            editableEmpty
                |> configSetEditable config
                |> RangeDateFilter
                |> flip (set column) model
                |> dispatchClear config.strategy

        Just (PeriodDateFilter config) ->
            editableEmpty
                |> configSetEditable config
                |> PeriodDateFilter
                |> flip (set column) model
                |> dispatchClear config.strategy

        Just (SelectFilter list config) ->
            editableEmpty
                |> configSetEditable config
                |> SelectFilter list
                |> flip (set column) model
                |> dispatchClear config.strategy

        Nothing ->
            ( model, Effect.none )



-- Filters combination


filtersReduce : (item -> Bool) -> (item -> Bool) -> (item -> Bool)
filtersReduce new old =
    \item -> new item && old item
