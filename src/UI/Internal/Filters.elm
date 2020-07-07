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
    | MultiTextModel (Editable (Dict Int String))
    | SingleDateModel (Editable Date)
    | RangeDateModel (Editable RangeDate)
    | PeriodDateModel (Editable PeriodDate)
    | SelectModel (Editable Int)


type alias Editable something =
    { current : Maybe something, applied : Maybe something }


type TimePeriod
    = On
    | Before
    | After


type Strategy msg value item
    = Local (item -> value -> Bool)
    | Remote msg


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


filtersPush : Filter msg item -> Filters msg item columns -> Filters msg item (T.Increase columns)
filtersPush filter accu =
    NArray.push filter accu


filterLocal : (item -> value -> Bool) -> Strategy msg value item
filterLocal isKept =
    Local isKept


filterRemote : msg -> Strategy msg value item
filterRemote applyMsg =
    Remote applyMsg



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
    Maybe String -> msg


singleTextEmpty : FilterModel
singleTextEmpty =
    SingleTextModel editableEmpty


localSingleTextFilter :
    Maybe String
    -> (item -> String)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
localSingleTextFilter initValue getStr accu =
    let
        applier item current =
            getStr item
                |> String.toLower
                |> String.contains (String.toLower current)
    in
    { initial = initValue, strategy = filterLocal applier }
        |> SingleTextFilter
        |> flip filtersPush accu


remoteSingleTextFilter :
    Maybe String
    -> (Maybe String -> msg)
    -> Filters msg item columns
    -> Filters msg item (T.Increase columns)
remoteSingleTextFilter initValue applyMsg accu =
    { initial = initValue, strategy = filterRemote applyMsg }
        |> SingleTextFilter
        |> flip filtersPush accu



-- MultiText


type alias MultiTextFilterConfig msg item =
    FilterConfig (List String) (MultiTextFilterStrategy msg item)


type alias MultiTextFilterStrategy msg item =
    Strategy (MultiTextFilterRemote msg) (List String) item


type alias MultiTextFilterRemote msg =
    List String -> msg



-- SingleDate


type alias Date =
    String


type alias SingleDateFilterConfig msg item =
    FilterConfig Date (SingleDateFilterStrategy msg item)


type alias SingleDateFilterStrategy msg item =
    Strategy (SingleDateFilterRemote msg) Date item


type alias SingleDateFilterRemote msg =
    Maybe Date -> msg



-- RangeDate


type alias RangeDate =
    { from : Date, to : Date }


type alias RangeDateFilterConfig msg item =
    FilterConfig RangeDate (RangeDateFilterStrategy msg item)


type alias RangeDateFilterStrategy msg item =
    Strategy (RangeDateFilterRemote msg) RangeDate item


type alias RangeDateFilterRemote msg =
    Maybe ( Date, Date ) -> msg



-- PeriodDate


type alias PeriodDate =
    { date : Date, timePeriod : TimePeriod }


type alias PeriodDateFilterConfig msg item =
    FilterConfig PeriodDate (PeriodDateFilterStrategy msg item)


type alias PeriodDateFilterStrategy msg item =
    Strategy (PeriodDateFilterRemote msg) PeriodDate item


type alias PeriodDateFilterRemote msg =
    Maybe ( String, TimePeriod ) -> msg



-- SelectFilter


type alias SelectFilterConfig msg item =
    FilterConfig Int (SelectFilterStrategy msg item)


type alias SelectFilterStrategy msg item =
    Strategy (SelectFilterRemote msg) Int item


type alias SelectFilterRemote msg =
    Maybe Int -> msg



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

        Just (MultiTextModel editable) ->
            editableApply editable
                |> MultiTextModel
                |> flip (Dict.insert column) model

        Just (SingleDateModel editable) ->
            editableApply editable
                |> SingleDateModel
                |> flip (Dict.insert column) model

        Just (RangeDateModel editable) ->
            editableApply editable
                |> RangeDateModel
                |> flip (Dict.insert column) model

        Just (PeriodDateModel editable) ->
            editableApply editable
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
        Just (SingleTextModel editable) ->
            editable

        _ ->
            editableEmpty


editMultiTextFilter : Int -> Int -> String -> Model -> Model
editMultiTextFilter column field value model =
    let
        newValue =
            case Dict.get column model of
                Just (MultiTextModel editable) ->
                    editable.current
                        |> Maybe.withDefault Dict.empty
                        |> Dict.insert field value
                        |> flip updateEditable editable

                _ ->
                    Dict.empty
                        |> Dict.insert field value
                        |> flip updateEditable editableEmpty
    in
    newValue
        |> MultiTextModel
        |> flip (Dict.insert column) model



-- Get Filters


getClearMsg : (Msg -> msg) -> Int -> Filter msg item -> msg
getClearMsg toExternalMsg index filter =
    case filter of
        SingleTextFilter { strategy } ->
            case strategy of
                Local _ ->
                    singleTextEmpty
                        |> Set index
                        |> toExternalMsg

                Remote applyMsg ->
                    applyMsg Nothing

        _ ->
            Debug.todo "TODO"


isEdited : Filter msg item -> Maybe FilterModel -> Bool
isEdited filter columnModel =
    case columnModel of
        Just (SingleTextModel { current }) ->
            current /= Nothing

        Just (MultiTextModel { current }) ->
            current /= Nothing

        Just (SingleDateModel { current }) ->
            current /= Nothing

        Just (RangeDateModel { current }) ->
            current /= Nothing

        Just (PeriodDateModel { current }) ->
            current /= Nothing

        Just (SelectModel { current }) ->
            current /= Nothing

        Nothing ->
            False


isApplied : Filter msg item -> Maybe FilterModel -> Bool
isApplied filter columnModel =
    case columnModel of
        Just (SingleTextModel { applied }) ->
            applied /= Nothing

        Just (MultiTextModel { applied }) ->
            applied /= Nothing

        Just (SingleDateModel { applied }) ->
            applied /= Nothing

        Just (RangeDateModel { applied }) ->
            applied /= Nothing

        Just (PeriodDateModel { applied }) ->
            applied /= Nothing

        Just (SelectModel { applied }) ->
            applied /= Nothing

        Nothing ->
            hasInitial filter


hasInitial : Filter msg item -> Bool
hasInitial filter =
    case filter of
        SingleTextFilter { initial } ->
            initial /= Nothing

        MultiTextFilter { initial } ->
            initial /= Nothing

        SingleDateFilter { initial } ->
            initial /= Nothing

        RangeDateFilter { initial } ->
            initial /= Nothing

        PeriodDateFilter { initial } ->
            initial /= Nothing

        SelectFilter { initial } ->
            initial /= Nothing


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
                        Just (MultiTextModel { applied }) ->
                            Maybe.andThen
                                (Dict.values >> applyIfNotEmpty applier)
                                applied

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
                        Just (RangeDateModel { applied }) ->
                            Maybe.map (flip applier) applied

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
                        Just (PeriodDateModel { applied }) ->
                            Maybe.map (flip applier) applied

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



-- Filters combination


filtersMerge :
    Maybe (Filter msg item)
    -> ( Int, Maybe FilterModel, Bool )
    -> Maybe (item -> Bool)
filtersMerge filter ( _, model, _ ) =
    Maybe.andThen (flip localFilterGet model) filter


filtersReduce : (item -> Bool) -> (item -> Bool) -> (item -> Bool)
filtersReduce new old =
    \item -> new item && old item
