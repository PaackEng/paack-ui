module UI.Internal.Tables.Filters exposing (..)

import Array exposing (Array)
import Task
import Time exposing (Posix)
import UI.Internal.Basics exposing (flip, maybeNotThen)
import UI.Internal.DateInput as DateInput exposing (DateInput(..), PeriodComparison(..), PeriodDate, RangeDate)
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



-- Internal Type


type Filter msg item
    = SingleTextFilter (SingleTextFilterConfig msg item)
    | MultiTextFilter (MultiTextFilterConfig msg item)
    | SingleDateFilter (SingleDateFilterConfig msg item)
    | RangeDateFilter (RangeDateFilterConfig msg item)
    | PeriodDateFilter (PeriodDateFilterConfig msg item)
    | SelectFilter (List String) (SelectFilterConfig msg item)


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


configSetEditable : FilterConfig msg value item -> Editable value -> FilterConfig msg value item
configSetEditable config newEditable =
    { config | editable = newEditable }



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


multiTextLocal :
    List String
    -> (item -> String)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
multiTextLocal initValue getStr accu =
    let
        applier item current =
            getStr item
                |> String.toLower
                |> String.contains (String.toLower current)

        strategy item valueArray =
            valueArray
                |> Array.toList
                |> List.any (applier item)
    in
    { editable = { applied = listToMaybeArray initValue, current = Nothing }
    , strategy = strategyLocal strategy
    }
        |> MultiTextFilter
        |> flip push accu


multiTextRemote :
    List String
    -> (List String -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
multiTextRemote initValue applyMsg accu =
    { editable = { applied = listToMaybeArray initValue, current = Nothing }
    , strategy =
        strategyRemote
            { applyMsg = Array.toList >> applyMsg
            , clearMsg = applyMsg []
            }
    }
        |> MultiTextFilter
        |> flip push accu


multiTextEdit : Int -> Int -> String -> Filters msg item columns -> Filters msg item columns
multiTextEdit column field value model =
    case get column model of
        Just (MultiTextFilter config) ->
            config.editable
                |> editableWithDefault Array.empty
                |> flexibleArray field value
                |> flip editableSetCurrent config.editable
                |> configSetEditable config
                |> MultiTextFilter
                |> flip (set column) model

        _ ->
            model


listToMaybeArray : List a -> Maybe (Array a)
listToMaybeArray list =
    if list == [] then
        Nothing

    else
        Just <| Array.fromList list


flexibleArray : Int -> String -> Array String -> Array String
flexibleArray index newValue array =
    if Array.length array == index then
        if String.isEmpty newValue then
            array

        else
            Array.push newValue array

    else
        array
            |> Array.set index newValue
            |> Array.filter (not << String.isEmpty)



-- SingleDate


type alias SingleDateFilterConfig msg item =
    FilterConfig msg DateInput item


singleDateLocal :
    Time.Zone
    -> Maybe Posix
    -> (item -> Posix)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
singleDateLocal timeZone initValue getPosix accu =
    let
        compare posix current =
            DateInput.isPosixEqual current timeZone posix

        applier item current =
            compare (getPosix item) current
    in
    { editable = { applied = Maybe.map (DateInput.fromPosix timeZone) initValue, current = Nothing }
    , strategy = strategyLocal applier
    }
        |> SingleDateFilter
        |> flip push accu


singleDateRemote :
    Time.Zone
    -> Maybe Posix
    -> (Maybe DateInput -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
singleDateRemote timeZone initValue applyMsg accu =
    { editable = { applied = Maybe.map (DateInput.fromPosix timeZone) initValue, current = Nothing }
    , strategy =
        strategyRemote
            { applyMsg = Just >> applyMsg
            , clearMsg = applyMsg Nothing
            }
    }
        |> SingleDateFilter
        |> flip push accu


singleDateEdit : Int -> String -> Filters msg item columns -> Filters msg item columns
singleDateEdit column value model =
    case get column model of
        Just (SingleDateFilter config) ->
            config.editable
                |> editableSetCurrent (DateInput.parseDD_MM_YYYY dateSeparator value)
                |> configSetEditable config
                |> SingleDateFilter
                |> flip (set column) model

        _ ->
            model



-- RangeDate


type alias RangeDateFilterConfig msg item =
    FilterConfig msg RangeDate item


rangeDateLocal :
    Time.Zone
    -> Maybe Posix
    -> Maybe Posix
    -> (item -> Posix)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
rangeDateLocal timeZone fromInit toInit getPosix accu =
    let
        compare posix { from, to } =
            DateInput.isPosixBetween timeZone posix from to

        applier item current =
            compare (getPosix item) current
    in
    { editable = { applied = rangeDateInit timeZone fromInit toInit, current = Nothing }
    , strategy = strategyLocal applier
    }
        |> RangeDateFilter
        |> flip push accu


rangeDateRemote :
    Time.Zone
    -> Maybe Posix
    -> Maybe Posix
    -> (Maybe RangeDate -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
rangeDateRemote timeZone fromInit toInit applyMsg accu =
    { editable = { applied = rangeDateInit timeZone fromInit toInit, current = Nothing }
    , strategy =
        strategyRemote
            { applyMsg = Just >> applyMsg
            , clearMsg = applyMsg Nothing
            }
    }
        |> RangeDateFilter
        |> flip push accu


rangeDateInit : Time.Zone -> Maybe Posix -> Maybe Posix -> Maybe RangeDate
rangeDateInit timeZone fromInit toInit =
    case ( fromInit, toInit ) of
        ( Just fromPosix, Just toPosix ) ->
            Just { from = DateInput.fromPosix timeZone fromPosix, to = DateInput.fromPosix timeZone toPosix }

        ( Just fromPosix, Nothing ) ->
            Just { from = DateInput.fromPosix timeZone fromPosix, to = DateInvalid "" }

        ( Nothing, Just toPosix ) ->
            Just { from = DateInvalid "", to = DateInput.fromPosix timeZone toPosix }

        ( Nothing, Nothing ) ->
            Nothing


rangeDateFromEdit : Int -> String -> Filters msg item columns -> Filters msg item columns
rangeDateFromEdit column value model =
    case get column model of
        Just (RangeDateFilter config) ->
            config.editable
                |> editableMapCurrent
                    (\maybeCurrent ->
                        { from = DateInput.parseDD_MM_YYYY dateSeparator value
                        , to =
                            maybeCurrent
                                |> Maybe.map .to
                                |> Maybe.withDefault (DateInvalid "")
                        }
                    )
                |> configSetEditable config
                |> RangeDateFilter
                |> flip (set column) model

        _ ->
            model


rangeDateToEdit : Int -> String -> Filters msg item columns -> Filters msg item columns
rangeDateToEdit column value model =
    case get column model of
        Just (RangeDateFilter config) ->
            config.editable
                |> editableMapCurrent
                    (\maybeCurrent ->
                        { from =
                            maybeCurrent
                                |> Maybe.map .from
                                |> Maybe.withDefault (DateInvalid "")
                        , to = DateInput.parseDD_MM_YYYY dateSeparator value
                        }
                    )
                |> configSetEditable config
                |> RangeDateFilter
                |> flip (set column) model

        _ ->
            model



-- PeriodDate


type alias PeriodDateFilterConfig msg item =
    FilterConfig msg PeriodDate item


periodDateInit : Time.Zone -> Maybe Posix -> Maybe PeriodComparison -> Maybe PeriodDate
periodDateInit timeZone posixInit periodInit =
    case ( posixInit, periodInit ) of
        ( Just posix, Just period ) ->
            Just { date = DateInput.fromPosix timeZone posix, comparison = period }

        ( Just posix, Nothing ) ->
            Just { date = DateInput.fromPosix timeZone posix, comparison = On }

        ( Nothing, Just period ) ->
            Just { date = DateInvalid "", comparison = period }

        ( Nothing, Nothing ) ->
            Nothing


periodDateLocal :
    Time.Zone
    -> Maybe Posix
    -> Maybe PeriodComparison
    -> (item -> Posix)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
periodDateLocal timeZone posixInit periodInit getPosix accu =
    let
        compare posix { date, comparison } =
            case comparison of
                On ->
                    DateInput.isPosixEqual date timeZone posix

                Before ->
                    DateInput.isPosixBefore date timeZone posix

                After ->
                    DateInput.isPosixAfter date timeZone posix

        applier item current =
            compare (getPosix item) current
    in
    { editable = { applied = periodDateInit timeZone posixInit periodInit, current = Nothing }
    , strategy = strategyLocal applier
    }
        |> PeriodDateFilter
        |> flip push accu


periodDateRemote :
    Time.Zone
    -> Maybe Posix
    -> Maybe PeriodComparison
    -> (Maybe PeriodDate -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
periodDateRemote timeZone posixInit periodInit applyMsg accu =
    { editable = { applied = periodDateInit timeZone posixInit periodInit, current = Nothing }
    , strategy =
        strategyRemote
            { applyMsg = Just >> applyMsg
            , clearMsg = applyMsg Nothing
            }
    }
        |> PeriodDateFilter
        |> flip push accu


periodDateEdit : Int -> String -> Filters msg item columns -> Filters msg item columns
periodDateEdit column value model =
    case get column model of
        Just (PeriodDateFilter config) ->
            config.editable
                |> editableMapCurrent
                    (\maybeCurrent ->
                        { date = DateInput.parseDD_MM_YYYY dateSeparator value
                        , comparison =
                            maybeCurrent
                                |> Maybe.map .comparison
                                |> Maybe.withDefault On
                        }
                    )
                |> configSetEditable config
                |> PeriodDateFilter
                |> flip (set column) model

        _ ->
            model


periodDateComparisonEdit : Int -> PeriodComparison -> Filters msg item columns -> Filters msg item columns
periodDateComparisonEdit column value model =
    case get column model of
        Just (PeriodDateFilter config) ->
            config.editable
                |> editableMapCurrent
                    (\maybeCurrent ->
                        { date =
                            maybeCurrent
                                |> Maybe.map .date
                                |> Maybe.withDefault (DateInvalid "")
                        , comparison = value
                        }
                    )
                |> configSetEditable config
                |> PeriodDateFilter
                |> flip (set column) model

        _ ->
            model



-- SelectFilter


type alias SelectFilterConfig msg item =
    FilterConfig msg Int item


selectLocal :
    List String
    -> Maybe Int
    -> (item -> Int -> Bool)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
selectLocal list initValue filter accu =
    { editable = { applied = initValue, current = Nothing }
    , strategy = strategyLocal filter
    }
        |> SelectFilter list
        |> flip push accu


selectRemote :
    List String
    -> Maybe Int
    -> (Maybe Int -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
selectRemote list initValue applyMsg accu =
    { editable = { applied = initValue, current = Nothing }
    , strategy =
        strategyRemote
            { applyMsg = Just >> applyMsg
            , clearMsg = applyMsg Nothing
            }
    }
        |> SelectFilter list
        |> flip push accu


selectEdit : Int -> Int -> Filters msg item columns -> Filters msg item columns
selectEdit column value model =
    case get column model of
        Just (SelectFilter list config) ->
            config.editable
                |> editableSetCurrent value
                |> configSetEditable config
                |> SelectFilter list
                |> flip (set column) model

        _ ->
            model



-- Update


update : Msg -> Filters msg item columns -> ( Filters msg item columns, Cmd msg )
update msg model =
    case msg of
        EditSingleText { column, value } ->
            ( singleTextEdit column value model, Cmd.none )

        EditMultiText { column, field, value } ->
            ( multiTextEdit column field value model, Cmd.none )

        EditSingleDate { column, value } ->
            ( singleDateEdit column value model, Cmd.none )

        EditRangeFromDate { column, value } ->
            ( rangeDateFromEdit column value model, Cmd.none )

        EditRangeToDate { column, value } ->
            ( rangeDateToEdit column value model, Cmd.none )

        EditPeriodDate { column, value } ->
            ( periodDateEdit column value model, Cmd.none )

        EditPeriodComparison { column, value } ->
            ( periodDateComparisonEdit column value model, Cmd.none )

        EditSelect { column, value } ->
            ( selectEdit column value model, Cmd.none )

        Apply column ->
            applyFilter column model

        Clear column ->
            filterClear column model


dispatchApply : Strategy msg value item -> value -> Filters msg item columns -> ( Filters msg item columns, Cmd msg )
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


dispatchClear : Strategy msg value item -> Filters msg item columns -> ( Filters msg item columns, Cmd msg )
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


applyShortcut :
    Filters msg item columns
    -> Int
    -> FilterConfig msg value item
    -> (FilterConfig msg value item -> Filter msg item)
    -> ( Filters msg item columns, Cmd msg )
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

        Just (SelectFilter list config) ->
            applyShortcut model column config (SelectFilter list)

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

        Just (SelectFilter list config) ->
            editableEmpty
                |> configSetEditable config
                |> SelectFilter list
                |> flip (set column) model
                |> dispatchClear config.strategy

        Nothing ->
            ( model, Cmd.none )


editableSetCurrent : value -> Editable value -> Editable value
editableSetCurrent new old =
    { old | current = Just new }


editableMapCurrent : (Maybe value -> value) -> Editable value -> Editable value
editableMapCurrent applier old =
    { old | current = Just <| applier <| maybeNotThen old.applied <| old.current }



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

        SelectFilter _ { editable } ->
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

        SelectFilter _ { editable } ->
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

        SelectFilter _ config ->
            localAppliedMap config



-- Filters combination


filtersReduce : (item -> Bool) -> (item -> Bool) -> (item -> Bool)
filtersReduce new old =
    \item -> new item && old item



-- Constant


dateSeparator : String
dateSeparator =
    "/"