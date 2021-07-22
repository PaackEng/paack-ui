module UI.Internal.Filter.Update exposing (Analytics(..), OutMsg, apply, emptyOutMsg, update)

import UI.Effect as Effect exposing (Effect)
import UI.Internal.Filter.Model exposing (..)
import UI.Internal.Filter.Msg exposing (Msg(..))
import UI.Internal.Filter.Sorter as Sorter


type alias OutMsg msg =
    { effects : Effect msg
    , closeDialog : Bool
    , registerAnalytics : Maybe Analytics
    , focusElement : Maybe String
    }


type Analytics
    = Applied
    | Cleared


apply : Filter msg item -> Maybe (Sorter.Status item) -> List item -> List item
apply filter sorting items =
    let
        filteredResults =
            case filterGet filter of
                Just filterFunction ->
                    List.filter filterFunction items

                Nothing ->
                    items
    in
    case sorting of
        Just ( Just direction, sorter ) ->
            Sorter.sort sorter direction filteredResults

        _ ->
            filteredResults


update : Msg -> Filter msg item -> ( Filter msg item, OutMsg msg )
update msg model =
    case msg of
        EditSingleText value ->
            ( singleTextEdit value model, emptyOutMsg )

        EditMultiText field value ->
            ( multiTextEdit field value model, emptyOutMsg )

        EditSingleDate value ->
            ( singleDateEdit value model, emptyOutMsg )

        EditRangeFromDate value ->
            ( rangeDateFromEdit value model, emptyOutMsg )

        EditRangeToDate value ->
            ( rangeDateToEdit value model, emptyOutMsg )

        EditPeriodDate value ->
            ( periodDateEdit value model, emptyOutMsg )

        EditPeriodComparison focusedElement value ->
            ( periodDateComparisonEdit value model, focusElementOutMsg focusedElement )

        EditSelect focusedElement value ->
            ( selectEdit value model, focusElementOutMsg focusedElement )

        DomFocusResult _ ->
            ( model, emptyOutMsg )

        Apply ->
            applyFilter model

        Clear ->
            filterClear model


emptyOutMsg : OutMsg msg
emptyOutMsg =
    { effects = Effect.none, closeDialog = False, registerAnalytics = Nothing, focusElement = Nothing }


focusElementOutMsg : String -> OutMsg msg
focusElementOutMsg element =
    { effects = Effect.none, closeDialog = False, registerAnalytics = Nothing, focusElement = Just element }


dispatchApply : Strategy msg value item -> value -> Filter msg item -> ( Filter msg item, OutMsg msg )
dispatchApply strategy value newModel =
    case strategy of
        Local _ ->
            ( newModel
            , { emptyOutMsg | closeDialog = True, registerAnalytics = Just Applied }
            )

        Remote { applyMsg } ->
            ( newModel
            , { effects = Effect.msgToCmd <| applyMsg value
              , closeDialog = True
              , registerAnalytics = Just Applied
              , focusElement = Nothing
              }
            )


dispatchClear : Strategy msg value item -> Filter msg item -> ( Filter msg item, OutMsg msg )
dispatchClear strategy newModel =
    case strategy of
        Local _ ->
            ( newModel
            , { emptyOutMsg | closeDialog = True, registerAnalytics = Just Cleared }
            )

        Remote { clearMsg } ->
            ( newModel
            , { effects = Effect.msgToCmd clearMsg
              , closeDialog = True
              , registerAnalytics = Just Cleared
              , focusElement = Nothing
              }
            )


applyShortcut :
    FilterConfig msg value item
    -> (FilterConfig msg value item -> Filter msg item)
    -> ( Filter msg item, OutMsg msg )
applyShortcut config constructor =
    case config.editable.current of
        Just newValue ->
            editableApply config.editable
                |> configSetEditable config
                |> constructor
                |> dispatchApply config.strategy newValue

        Nothing ->
            ( constructor config, emptyOutMsg )


applyFilter : Filter msg item -> ( Filter msg item, OutMsg msg )
applyFilter filter =
    case filter of
        SingleTextFilter config ->
            applyShortcut config SingleTextFilter

        MultiTextFilter config ->
            applyShortcut config MultiTextFilter

        SingleDateFilter config ->
            applyShortcut config SingleDateFilter

        RangeDateFilter config ->
            applyShortcut config RangeDateFilter

        PeriodDateFilter data config ->
            applyShortcut config (PeriodDateFilter data)

        SelectFilter data config ->
            applyShortcut config (SelectFilter data)


filterClear : Filter msg item -> ( Filter msg item, OutMsg msg )
filterClear filter =
    case filter of
        SingleTextFilter config ->
            editableEmpty
                |> configSetEditable config
                |> SingleTextFilter
                |> dispatchClear config.strategy

        MultiTextFilter config ->
            editableEmpty
                |> configSetEditable config
                |> MultiTextFilter
                |> dispatchClear config.strategy

        SingleDateFilter config ->
            editableEmpty
                |> configSetEditable config
                |> SingleDateFilter
                |> dispatchClear config.strategy

        RangeDateFilter config ->
            editableEmpty
                |> configSetEditable config
                |> RangeDateFilter
                |> dispatchClear config.strategy

        PeriodDateFilter data config ->
            editableEmpty
                |> configSetEditable config
                |> PeriodDateFilter data
                |> dispatchClear config.strategy

        SelectFilter data config ->
            editableEmpty
                |> configSetEditable config
                |> SelectFilter data
                |> dispatchClear config.strategy
