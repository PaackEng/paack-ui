module UI.Internal.Filters exposing (..)

import Dict exposing (Dict)
import UI.Internal.Basics exposing (flip, maybeNotThen)
import UI.Internal.NArray as NArray exposing (NArray)
import UI.Internal.TypeNumbers as T


type Msg
    = EditSingleText { column : Int, value : String }
    | EditMultiText { column : Int, field : Int, value : String }
    | Apply Int
    | Set Int FilterModel


type alias Filters msg item columns =
    NArray (Filter msg item) columns


type Filter msg item
    = SingleTextFilter (SingleTextFilterConfig msg item)
    | MultiTextFilter (MultiTextFilterConfig msg item)
    | SingleDateFilter (SingleDateFilterConfig msg item)
    | RangeDateFilter (RangeDateFilterConfig msg item)
    | PeriodDateFilter (PeriodDateFilterConfig msg item)
    | SelectFilter (SelectFilterConfig msg item)


type alias Model =
    Dict Int FilterModel


type FilterModel
    = SingleTextModel (Editable String)
    | MultiTextModel (Dict Int (Editable String))
    | SingleDateModel (Editable Date)
    | RangeDateModel { from : Editable Date, to : Editable Date }
    | PeriodDateModel { date : Editable Date, timePeriod : Editable TimePeriod }
    | SelectModel (Editable Int)


type alias Editable something =
    { current : Maybe something, applied : Maybe something }


type TimePeriod
    = On
    | Before
    | After


type Strategy msgs value item
    = Local (item -> value -> Bool)
    | Remote msgs


init : Model
init =
    Dict.empty


get : Int -> Model -> Maybe FilterModel
get column model =
    Dict.get column model


editableEmpty : Editable something
editableEmpty =
    { current = Nothing, applied = Nothing }


editableInit : Maybe data -> Editable data
editableInit data =
    { current = Nothing, applied = data }


editableDefault : data -> Editable data -> data
editableDefault default { current, applied } =
    current
        |> maybeNotThen applied
        |> Maybe.withDefault default


editableApply : Editable data -> Editable data
editableApply { current } =
    { current = Nothing, applied = current }


filtersEmpty : Filters msg item T.Zero
filtersEmpty =
    NArray.empty


filtersPush : Filters.Filter msg item -> Filters msg item columns -> Filters msg item (T.Increase columns)
filtersPush filter accu =
    NArray.push filter accu


filterRemote : msgs -> Strategy msgs value item
filterRemote msgs =
    Filters.Remote msgs



-- Configs


type alias FilterConfig data strategy =
    { initial : Maybe data
    , strategy : strategy
    }



-- SingleText


type alias SingleTextFilterConfig msg item =
    FilterConfig String (SingleTextFilterStrategy msg item)


type alias SingleTextFilterStrategy msg item =
    Strategy (SingleTextFilterRemote msg) String item


type alias SingleTextFilterRemote msg =
    { editMsg : String -> msg
    }


singleTextEmpty : FilterModel
singleTextEmpty =
    SingleTextModel editableEmpty


localSingleTextFilter :
    Maybe String
    -> (item -> String)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
localSingleTextFilter initValue get accu =
    let
        applier item current =
            get item
                |> String.toLower
                |> String.contains (String.toLower current)
    in
    { initial = initValue, strategy = filterLocal applier }
        |> Filters.SingleTextFilter
        |> flip filtersPush accu


filterLocal : (item -> value -> Bool) -> Strategy msgs value item
filterLocal isKept =
    Filters.Local isKept


remoteSingleTextFilter :
    Maybe String
    -> (String -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
remoteSingleTextFilter initValue editMsg accu =
    let
        msgs =
            { editMsg = editMsg }
    in
    { initial = initValue, strategy = filterRemote msgs }
        |> Filters.SingleTextFilter
        |> flip filtersPush accu



-- MultiText


type alias MultiTextFilterConfig msg item =
    FilterConfig (List String) (MultiTextFilterStrategy msg item)


type alias MultiTextFilterStrategy msg item =
    Strategy (MultiTextFilterRemote msg) (List String) item


type alias MultiTextFilterRemote msg =
    { editMsg : Int -> String -> msg
    }



-- SingleDate


type alias Date =
    String


type alias SingleDateFilterConfig msg item =
    FilterConfig Date (SingleDateFilterStrategy msg item)


type alias SingleDateFilterStrategy msg item =
    Strategy (SingleDateFilterRemote msg) Date item


type alias SingleDateFilterRemote msg =
    { editMsg : Date -> msg
    }



-- RangeDate


type alias RangeDate =
    { from : Date, to : String }


type alias RangeDateFilterConfig msg item =
    FilterConfig RangeDate (RangeDateFilterStrategy msg item)


type alias RangeDateFilterStrategy msg item =
    Strategy (RangeDateFilterRemote msg) RangeDate item


type alias RangeDateFilterRemote msg =
    { fromEditMsg : Date -> msg
    , toEditMsg : String -> msg
    }



-- PeriodDate


type alias PeriodDate =
    { date : Date, timePeriod : TimePeriod }


type alias PeriodDateFilterConfig msg item =
    FilterConfig PeriodDate (PeriodDateFilterStrategy msg item)


type alias PeriodDateFilterStrategy msg item =
    Strategy (PeriodDateFilterRemote msg) PeriodDate item


type alias PeriodDateFilterRemote msg =
    { dateEditMsg : String -> msg
    , periodEditMsg : TimePeriod -> msg
    }



-- SelectFilter


type alias SelectFilterConfig msg item =
    FilterConfig Int (SelectFilterStrategy msg item)


type alias SelectFilterStrategy msg item =
    Strategy (SelectFilterRemote msg) Int item


type alias SelectFilterRemote msg =
    { selectMsg : Int -> msg
    }



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        EditSingleText { column, value } ->
            model
                |> getSingleTextEditable column
                |> updateEditable value
                |> SingleTextModel
                |> flip (Dict.insert column) model

        EditMultiText { column, field, value } ->
            editMultiTextFilter column field value model

        Apply column ->
            applyFilter column model

        Set column emptyModel ->
            Dict.insert column emptyModel model


applyFilter : Int -> Model -> Model
applyFilter column model =
    case Dict.get column model of
        Just (SingleTextModel editable) ->
            editableApply editable
                |> SingleTextModel
                |> flip (Dict.insert column) model

        Just (MultiTextModel dict) ->
            dict
                |> Dict.map (always editableApply)
                |> MultiTextModel
                |> flip (Dict.insert column) model

        Just (SingleDateModel editable) ->
            editableApply editable
                |> SingleDateModel
                |> flip (Dict.insert column) model

        Just (RangeDateModel { from, to }) ->
            { from = editableApply from, to = editableApply to }
                |> RangeDateModel
                |> flip (Dict.insert column) model

        Just (PeriodDateModel { date, timePeriod }) ->
            { date = editableApply date, timePeriod = editableApply timePeriod }
                |> PeriodDateModel
                |> flip (Dict.insert column) model

        Just (SelectModel editable) ->
            editableApply editable
                |> SelectModel
                |> flip (Dict.insert column) model

        Nothing ->
            model


updateEditable : value -> Editable value -> Editable value
updateEditable new old =
    { old | current = Just new }


getSingleTextEditable : Int -> Model -> Editable String
getSingleTextEditable column model =
    case Dict.get column model of
        Just (SingleTextModel data) ->
            data

        _ ->
            editableEmpty


editMultiTextFilter : Int -> Int -> String -> Model -> Model
editMultiTextFilter column field value model =
    let
        oldDict =
            getMultiTextDict column model

        oldEditable =
            oldDict
                |> Dict.get field
                |> Maybe.withDefault editableEmpty
    in
    oldEditable
        |> updateEditable value
        |> flip (Dict.insert field) oldDict
        |> MultiTextModel
        |> flip (Dict.insert column) model


getMultiTextDict : Int -> Model -> Dict Int (Editable String)
getMultiTextDict column model =
    case Dict.get column model of
        Just (MultiTextModel data) ->
            data

        _ ->
            Dict.empty



-- Get Filters


localFilterGet : Filter msg item -> Maybe FilterModel -> Maybe (item -> Bool)
localFilterGet filter maybeModel =
    case filter of
        SingleTextFilter { initial, strategy } ->
            case strategy of
                Local applier ->
                    case maybeModel of
                        Just (SingleTextModel { applied }) ->
                            Maybe.map (flip applier) applied

                        Nothing ->
                            Maybe.map (flip applier) initial

                        _ ->
                            Nothing

                Remote _ ->
                    Nothing

        MultiTextFilter { initial, strategy } ->
            case strategy of
                Local applier ->
                    case maybeModel of
                        Just (MultiTextModel dict) ->
                            dict
                                |> Dict.toList
                                |> List.filterMap (Tuple.second >> .applied)
                                |> applyIfNotEmpty applier

                        Nothing ->
                            Maybe.map (flip applier) initial

                        _ ->
                            Nothing

                Remote _ ->
                    Nothing

        SingleDateFilter { initial, strategy } ->
            case strategy of
                Local applier ->
                    case maybeModel of
                        Just (SingleDateModel { applied }) ->
                            Maybe.map (flip applier) applied

                        Nothing ->
                            Maybe.map (flip applier) initial

                        _ ->
                            Nothing

                Remote _ ->
                    Nothing

        RangeDateFilter { initial, strategy } ->
            case strategy of
                Local applier ->
                    case maybeModel of
                        Just (RangeDateModel { from, to }) ->
                            Maybe.map2
                                (\from_ to_ -> flip applier { from = from_, to = to_ })
                                from.applied
                                to.applied

                        Nothing ->
                            Maybe.map (flip applier) initial

                        _ ->
                            Nothing

                Remote _ ->
                    Nothing

        PeriodDateFilter { initial, strategy } ->
            case strategy of
                Local applier ->
                    case maybeModel of
                        Just (PeriodDateModel { date, timePeriod }) ->
                            Maybe.map2
                                (\date_ timePeriod_ -> flip applier { date = date_, timePeriod = timePeriod_ })
                                date.applied
                                timePeriod.applied

                        Nothing ->
                            Maybe.map (flip applier) initial

                        _ ->
                            Nothing

                Remote _ ->
                    Nothing

        SelectFilter { initial, strategy } ->
            case strategy of
                Local applier ->
                    case maybeModel of
                        Just (SelectModel { applied }) ->
                            Maybe.map (flip applier) applied

                        Nothing ->
                            Maybe.map (flip applier) initial

                        _ ->
                            Nothing

                Remote _ ->
                    Nothing


applyIfNotEmpty : (item -> List value -> Bool) -> List value -> Maybe (item -> Bool)
applyIfNotEmpty applier list =
    if List.length list /= 0 then
        Just <| flip applier list

    else
        Nothing
