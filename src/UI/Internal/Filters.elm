module UI.Internal.Filters exposing (..)

import Array exposing (Array)
import Task
import UI.Internal.Basics exposing (flip, maybeNotThen)
import UI.Internal.NArray as NArray exposing (NArray)
import UI.Internal.TypeNumbers as T


type Msg
    = EditSingleText { column : Int, value : String }
    | EditMultiText { column : Int, field : Int, value : String }
    | Apply Int
    | Clear Int


type alias Filters msg item columns =
    NArray (Filter msg item) columns



-- Internal Type


type Filter msg item
    = SingleTextFilter (SingleTextFilterConfig msg item)
    | MultiTextFilter (MultiTextFilterConfig msg item)
    | SingleDateFilter (SingleDateFilterConfig msg item)
    | RangeDateFilter (RangeDateFilterConfig msg item)
    | PeriodDateFilter (PeriodDateFilterConfig msg item)
    | SelectFilter (SelectFilterConfig msg item)


type alias Editable value =
    { current : Maybe value
    , applied : Maybe value
    }


type alias FilterConfig msg value item =
    { editable : Editable value
    , strategy : Strategy msg value item
    }


type Strategy msg value item
    = Local (item -> value -> Bool)
    | Remote (RemoteMessages msg value)


type alias RemoteMessages msg value =
    { applyMsg : value -> msg, clearMsg : msg }


configSetEditable config newEditable =
    { config | editable = newEditable }



-- Type-helpers


type alias RangeDate =
    { from : Date, to : Date }


type alias Date =
    String


type TimePeriod
    = On
    | Before
    | After


type alias PeriodDate =
    { date : Date, timePeriod : TimePeriod }



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



-- Editable


editableEmpty : Editable something
editableEmpty =
    { current = Nothing, applied = Nothing }


editableWithDefault : data -> Editable data -> data
editableWithDefault default { current, applied } =
    current
        |> maybeNotThen applied
        |> Maybe.withDefault default


editableApply : Editable data -> Editable data
editableApply { current } =
    { current = Nothing, applied = current }



-- Strategy


strategyLocal : (item -> value -> Bool) -> Strategy msg value item
strategyLocal isKept =
    Local isKept


strategyRemote : RemoteMessages msg value -> Strategy msg value item
strategyRemote messages =
    Remote messages



-- SingleText


type alias SingleTextFilterConfig msg item =
    FilterConfig msg String item


singleTextLocal :
    Maybe String
    -> (item -> String)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
singleTextLocal initValue getStr accu =
    let
        applier item current =
            getStr item
                |> String.toLower
                |> String.contains (String.toLower current)
    in
    { editable = { applied = initValue, current = Nothing }
    , strategy = strategyLocal applier
    }
        |> SingleTextFilter
        |> flip push accu


singleTextRemote :
    Maybe String
    -> (Maybe String -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
singleTextRemote initValue applyMsg accu =
    { editable = { applied = initValue, current = Nothing }
    , strategy =
        strategyRemote
            { applyMsg = Just >> applyMsg
            , clearMsg = applyMsg Nothing
            }
    }
        |> SingleTextFilter
        |> flip push accu


singleTextEdit : Int -> String -> Filters msg item columns -> Filters msg item columns
singleTextEdit column value model =
    case get column model of
        Just (SingleTextFilter config) ->
            config.editable
                |> editableSetCurrent value
                |> configSetEditable config
                |> SingleTextFilter
                |> flip (set column) model

        _ ->
            model



-- MultiText


type alias MultiTextFilterConfig msg item =
    FilterConfig msg (Array String) item


multiTextEdit : Int -> Int -> String -> Filters msg item columns -> Filters msg item columns
multiTextEdit column field value model =
    case get column model of
        Just (MultiTextFilter config) ->
            config.editable
                |> editableWithDefault Array.empty
                |> Array.set field value
                |> flip editableSetCurrent config.editable
                |> configSetEditable config
                |> MultiTextFilter
                |> flip (set column) model

        _ ->
            model



-- SingleDate


type alias SingleDateFilterConfig msg item =
    FilterConfig msg Date item



-- RangeDate


type alias RangeDateFilterConfig msg item =
    FilterConfig msg RangeDate item



-- PeriodDate


type alias PeriodDateFilterConfig msg item =
    FilterConfig msg PeriodDate item



-- SelectFilter


type alias SelectFilterConfig msg item =
    FilterConfig msg Int item



-- Update


update : Msg -> Filters msg item columns -> ( Filters msg item columns, Cmd msg )
update msg model =
    case msg of
        EditSingleText { column, value } ->
            ( singleTextEdit column value model, Cmd.none )

        EditMultiText { column, field, value } ->
            ( multiTextEdit column field value model, Cmd.none )

        Apply column ->
            applyFilter column model

        Clear column ->
            filterClear column model


dispatchApply strategy value newModel =
    case strategy of
        Local _ ->
            ( newModel
            , Cmd.none
            )

        Remote { applyMsg } ->
            ( newModel
            , applyMsg value
                |> Task.succeed
                |> Task.perform identity
            )


dispatchClear strategy newModel =
    case strategy of
        Local _ ->
            ( newModel
            , Cmd.none
            )

        Remote { clearMsg } ->
            ( newModel
            , clearMsg
                |> Task.succeed
                |> Task.perform identity
            )


applyShortcut model column config constructor =
    case config.editable.current of
        Just newValue ->
            editableApply config.editable
                |> configSetEditable config
                |> constructor
                |> flip (set column) model
                |> dispatchApply config.strategy newValue

        Nothing ->
            ( model, Cmd.none )


applyFilter : Int -> Filters msg item columns -> ( Filters msg item columns, Cmd msg )
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

        Just (SelectFilter config) ->
            applyShortcut model column config SelectFilter

        Nothing ->
            ( model, Cmd.none )


filterClear : Int -> Filters msg item columns -> ( Filters msg item columns, Cmd msg )
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

        Just (SelectFilter config) ->
            editableEmpty
                |> configSetEditable config
                |> SelectFilter
                |> flip (set column) model
                |> dispatchClear config.strategy

        Nothing ->
            ( model, Cmd.none )


editableSetCurrent : value -> Editable value -> Editable value
editableSetCurrent new old =
    { old | current = Just new }



-- Get Filters


isEdited : Filter msg item -> Bool
isEdited filter =
    case filter of
        SingleTextFilter { editable } ->
            editable.current /= Nothing

        MultiTextFilter { editable } ->
            editable.current /= Nothing

        SingleDateFilter { editable } ->
            editable.current /= Nothing

        RangeDateFilter { editable } ->
            editable.current /= Nothing

        PeriodDateFilter { editable } ->
            editable.current /= Nothing

        SelectFilter { editable } ->
            editable.current /= Nothing


isApplied : Filter msg item -> Bool
isApplied filter =
    case filter of
        SingleTextFilter { editable } ->
            editable.applied /= Nothing

        MultiTextFilter { editable } ->
            editable.applied /= Nothing

        SingleDateFilter { editable } ->
            editable.applied /= Nothing

        RangeDateFilter { editable } ->
            editable.applied /= Nothing

        PeriodDateFilter { editable } ->
            editable.applied /= Nothing

        SelectFilter { editable } ->
            editable.applied /= Nothing


localAppliedMap : FilterConfig msg value item -> Maybe (item -> Bool)
localAppliedMap { editable, strategy } =
    case strategy of
        Local applier ->
            Maybe.map (flip applier) editable.applied

        Remote _ ->
            Nothing


filterGet : Filter msg item -> Maybe (item -> Bool)
filterGet filter =
    case filter of
        SingleTextFilter config ->
            localAppliedMap config

        MultiTextFilter config ->
            localAppliedMap config

        SingleDateFilter config ->
            localAppliedMap config

        RangeDateFilter config ->
            localAppliedMap config

        PeriodDateFilter config ->
            localAppliedMap config

        SelectFilter config ->
            localAppliedMap config



-- Filters combination


filtersReduce : (item -> Bool) -> (item -> Bool) -> (item -> Bool)
filtersReduce new old =
    \item -> new item && old item
