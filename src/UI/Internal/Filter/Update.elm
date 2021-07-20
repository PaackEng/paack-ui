module UI.Internal.Filter.Update exposing (Analytics(..), OutMsg, emptyOutMsg, update)

import UI.Effect as Effect exposing (Effect)
import UI.Internal.Filter.Model exposing (..)
import UI.Internal.Filter.Msg exposing (Msg(..))


type alias OutMsg msg =
    { effects : Effect msg
    , closeDialog : Bool
    , registerAnalytics : Maybe Analytics
    }


type Analytics
    = Applied
    | Cleared


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

        EditPeriodComparison value ->
            ( periodDateComparisonEdit value model, emptyOutMsg )

        EditSelect value ->
            ( selectEdit value model, emptyOutMsg )

        Apply ->
            applyFilter model

        Clear ->
            filterClear model


emptyOutMsg : OutMsg msg
emptyOutMsg =
    { effects = Effect.none, closeDialog = False, registerAnalytics = Nothing }


dispatchApply : Strategy msg value item -> value -> Filter msg item -> ( Filter msg item, OutMsg msg )
dispatchApply strategy value newModel =
    case strategy of
        Local _ ->
            ( newModel
            , { effects = Effect.none, closeDialog = True, registerAnalytics = Just Applied }
            )

        Remote { applyMsg } ->
            ( newModel
            , { effects = Effect.msgToCmd <| applyMsg value
              , closeDialog = True
              , registerAnalytics = Just Applied
              }
            )


dispatchClear : Strategy msg value item -> Filter msg item -> ( Filter msg item, OutMsg msg )
dispatchClear strategy newModel =
    case strategy of
        Local _ ->
            ( newModel
            , { effects = Effect.none, closeDialog = True, registerAnalytics = Just Cleared }
            )

        Remote { clearMsg } ->
            ( newModel
            , { effects = Effect.msgToCmd clearMsg
              , closeDialog = True
              , registerAnalytics = Just Cleared
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

        PeriodDateFilter config ->
            applyShortcut config PeriodDateFilter

        SelectFilter list config ->
            applyShortcut config (SelectFilter list)


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

        PeriodDateFilter config ->
            editableEmpty
                |> configSetEditable config
                |> PeriodDateFilter
                |> dispatchClear config.strategy

        SelectFilter list config ->
            editableEmpty
                |> configSetEditable config
                |> SelectFilter list
                |> dispatchClear config.strategy
