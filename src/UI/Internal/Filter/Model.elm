module UI.Internal.Filter.Model exposing (..)

import Array exposing (Array)
import Time exposing (Posix)
import UI.Internal.Basics exposing (flip, maybeNotThen)
import UI.Internal.DateInput as DateInput exposing (DateInput(..), PeriodComparison(..), PeriodDate, RangeDate)



-- Constant


dateSeparator : String
dateSeparator =
    "/"



-- Main Type


type Filter msg item
    = SingleTextFilter (SingleTextFilterConfig msg item)
    | MultiTextFilter (MultiTextFilterConfig msg item)
    | SingleDateFilter (SingleDateFilterConfig msg item)
    | RangeDateFilter (RangeDateFilterConfig msg item)
    | PeriodDateFilter { domId : String } (PeriodDateFilterConfig msg item)
    | SelectFilter { domId : String, items : List String } (SelectFilterConfig msg item)


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


editableSetCurrent : value -> Editable value -> Editable value
editableSetCurrent new old =
    { old | current = Just new }


editableMapCurrent : (Maybe value -> value) -> Editable value -> Editable value
editableMapCurrent applier old =
    { old | current = Just <| applier <| maybeNotThen old.applied <| old.current }



-- Strategy


strategyLocal : (item -> value -> Bool) -> Strategy msg value item
strategyLocal isKept =
    Local isKept


strategyRemote : RemoteMessages msg value -> Strategy msg value item
strategyRemote messages =
    Remote messages



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

        PeriodDateFilter _ { editable } ->
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

        PeriodDateFilter _ { editable } ->
            editable.applied /= Nothing

        SelectFilter _ { editable } ->
            editable.applied /= Nothing


appliedLength : Filter msg item -> Maybe Int
appliedLength filter =
    case filter of
        SingleTextFilter { editable } ->
            Maybe.map (always 1) editable.applied

        MultiTextFilter { editable } ->
            Maybe.map Array.length editable.applied

        SingleDateFilter { editable } ->
            Maybe.map (always 1) editable.applied

        RangeDateFilter { editable } ->
            Maybe.map (always 1) editable.applied

        PeriodDateFilter _ { editable } ->
            Maybe.map (always 1) editable.applied

        SelectFilter _ { editable } ->
            Maybe.map (always 1) editable.applied


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

        PeriodDateFilter _ config ->
            localAppliedMap config

        SelectFilter _ config ->
            localAppliedMap config



-- SingleText


type alias SingleTextFilterConfig msg item =
    FilterConfig msg String item


singleTextLocal :
    Maybe String
    -> (item -> String)
    -> Filter msg item
singleTextLocal initValue getStr =
    let
        applier item current =
            getStr item
                |> String.toLower
                |> String.contains (String.toLower current)
    in
    SingleTextFilter
        { editable = { applied = initValue, current = Nothing }
        , strategy = strategyLocal applier
        }


singleTextRemote :
    Maybe String
    -> (Maybe String -> msg)
    -> Filter msg item
singleTextRemote initValue applyMsg =
    SingleTextFilter
        { editable = { applied = initValue, current = Nothing }
        , strategy =
            strategyRemote
                { applyMsg = Just >> applyMsg
                , clearMsg = applyMsg Nothing
                }
        }


singleTextEdit : String -> Filter msg item -> Filter msg item
singleTextEdit value filter =
    case filter of
        SingleTextFilter config ->
            config.editable
                |> editableSetCurrent value
                |> configSetEditable config
                |> SingleTextFilter

        _ ->
            filter



-- MultiText


type alias MultiTextFilterConfig msg item =
    FilterConfig msg (Array String) item


multiTextLocal :
    List String
    -> (item -> String)
    -> Filter msg item
multiTextLocal initValue getStr =
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
    MultiTextFilter
        { editable = { applied = listToMaybeArray initValue, current = Nothing }
        , strategy = strategyLocal strategy
        }


multiTextRemote :
    List String
    -> (List String -> msg)
    -> Filter msg item
multiTextRemote initValue applyMsg =
    MultiTextFilter
        { editable = { applied = listToMaybeArray initValue, current = Nothing }
        , strategy =
            strategyRemote
                { applyMsg = Array.toList >> applyMsg
                , clearMsg = applyMsg []
                }
        }


multiTextEdit : Int -> String -> Filter msg item -> Filter msg item
multiTextEdit field value filter =
    case filter of
        MultiTextFilter config ->
            config.editable
                |> editableWithDefault Array.empty
                |> flexibleArray field value
                |> flip editableSetCurrent config.editable
                |> configSetEditable config
                |> MultiTextFilter

        _ ->
            filter


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
    -> Filter msg item
singleDateLocal timeZone initValue getPosix =
    let
        compare posix current =
            DateInput.isPosixEqual current timeZone posix

        applier item current =
            compare (getPosix item) current
    in
    SingleDateFilter
        { editable = { applied = Maybe.map (DateInput.fromPosix timeZone) initValue, current = Nothing }
        , strategy = strategyLocal applier
        }


singleDateRemote :
    Time.Zone
    -> Maybe Posix
    -> (Maybe DateInput -> msg)
    -> Filter msg item
singleDateRemote timeZone initValue applyMsg =
    SingleDateFilter
        { editable = { applied = Maybe.map (DateInput.fromPosix timeZone) initValue, current = Nothing }
        , strategy =
            strategyRemote
                { applyMsg = Just >> applyMsg
                , clearMsg = applyMsg Nothing
                }
        }


singleDateEdit : String -> Filter msg item -> Filter msg item
singleDateEdit value filter =
    case filter of
        SingleDateFilter config ->
            config.editable
                |> editableSetCurrent (DateInput.parseDD_MM_YYYY dateSeparator value)
                |> configSetEditable config
                |> SingleDateFilter

        _ ->
            filter



-- RangeDate


type alias RangeDateFilterConfig msg item =
    FilterConfig msg RangeDate item


rangeDateLocal :
    Time.Zone
    -> Maybe Posix
    -> Maybe Posix
    -> (item -> Posix)
    -> Filter msg item
rangeDateLocal timeZone fromInit toInit getPosix =
    let
        compare posix { from, to } =
            DateInput.isPosixBetween timeZone posix from to

        applier item current =
            compare (getPosix item) current
    in
    RangeDateFilter
        { editable = { applied = rangeDateInit timeZone fromInit toInit, current = Nothing }
        , strategy = strategyLocal applier
        }


rangeDateRemote :
    Time.Zone
    -> Maybe Posix
    -> Maybe Posix
    -> (Maybe RangeDate -> msg)
    -> Filter msg item
rangeDateRemote timeZone fromInit toInit applyMsg =
    RangeDateFilter
        { editable = { applied = rangeDateInit timeZone fromInit toInit, current = Nothing }
        , strategy =
            strategyRemote
                { applyMsg = Just >> applyMsg
                , clearMsg = applyMsg Nothing
                }
        }


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


rangeDateFromEdit : String -> Filter msg item -> Filter msg item
rangeDateFromEdit value filter =
    case filter of
        RangeDateFilter config ->
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

        _ ->
            filter


rangeDateToEdit : String -> Filter msg item -> Filter msg item
rangeDateToEdit value filter =
    case filter of
        RangeDateFilter config ->
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

        _ ->
            filter



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
    String
    -> Time.Zone
    -> Maybe Posix
    -> Maybe PeriodComparison
    -> (item -> Posix)
    -> Filter msg item
periodDateLocal domId timeZone posixInit periodInit getPosix =
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
    PeriodDateFilter
        { domId = domId }
        { editable = { applied = periodDateInit timeZone posixInit periodInit, current = Nothing }
        , strategy = strategyLocal applier
        }


elmOrderToPeriodComparison : Order -> PeriodComparison
elmOrderToPeriodComparison order =
    case order of
        LT ->
            Before

        EQ ->
            On

        GT ->
            After


periodDateRemote :
    String
    -> Time.Zone
    -> Maybe Posix
    -> Maybe PeriodComparison
    -> (Maybe PeriodDate -> msg)
    -> Filter msg item
periodDateRemote domId timeZone posixInit periodInit applyMsg =
    PeriodDateFilter
        { domId = domId }
        { editable = { applied = periodDateInit timeZone posixInit periodInit, current = Nothing }
        , strategy =
            strategyRemote
                { applyMsg = Just >> applyMsg
                , clearMsg = applyMsg Nothing
                }
        }


periodDateEdit : String -> Filter msg item -> Filter msg item
periodDateEdit value filter =
    case filter of
        PeriodDateFilter radioConfig config ->
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
                |> PeriodDateFilter radioConfig

        _ ->
            filter


periodDateComparisonEdit : PeriodComparison -> Filter msg item -> Filter msg item
periodDateComparisonEdit value filter =
    case filter of
        PeriodDateFilter radioConfig config ->
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
                |> PeriodDateFilter radioConfig

        _ ->
            filter



-- SelectFilter


type alias SelectFilterConfig msg item =
    FilterConfig msg Int item


selectLocal :
    String
    -> List String
    -> Maybe Int
    -> (item -> Int -> Bool)
    -> Filter msg item
selectLocal domId items initValue filter =
    SelectFilter { domId = domId, items = items }
        { editable = { applied = initValue, current = Nothing }
        , strategy = strategyLocal filter
        }


selectRemote :
    String
    -> List String
    -> Maybe Int
    -> (Maybe Int -> msg)
    -> Filter msg item
selectRemote domId items initValue applyMsg =
    SelectFilter { domId = domId, items = items }
        { editable = { applied = initValue, current = Nothing }
        , strategy =
            strategyRemote
                { applyMsg = Just >> applyMsg
                , clearMsg = applyMsg Nothing
                }
        }


selectEdit : Int -> Filter msg item -> Filter msg item
selectEdit value filter =
    case filter of
        SelectFilter list config ->
            config.editable
                |> editableSetCurrent value
                |> configSetEditable config
                |> SelectFilter list

        _ ->
            filter
