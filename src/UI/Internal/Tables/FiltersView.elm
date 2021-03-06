module UI.Internal.Tables.FiltersView exposing (Config, header, headerSelectToggle)

-- WARNING: Don't use any other Size.* beyond "contextSize"

import Array exposing (Array)
import Element exposing (Element, fill, minimum, px)
import Element.Background as Background
import UI.Button as Button
import UI.Icon as Icon
import UI.Internal.Basics exposing (maybeNotThen)
import UI.Internal.Colors as Colors
import UI.Internal.DateInput as DateInput exposing (DateInput(..), PeriodComparison(..), PeriodDate, RangeDate)
import UI.Internal.Filter.Model as Filter exposing (Filter)
import UI.Internal.Filter.Sorter as Sorter exposing (SortingDirection(..))
import UI.Internal.Filter.View as FilterV2
import UI.Internal.Primitives as Primitives
import UI.Internal.RenderConfig as RenderConfig exposing (localeTerms)
import UI.Internal.Size as Size exposing (Size)
import UI.Internal.Tables.Filters as Filters
import UI.Internal.Tables.Sorters as Sorters
import UI.Radio as Radio
import UI.RenderConfig exposing (RenderConfig)
import UI.TextField as TextField exposing (TextField)
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element


type alias Config msg =
    { openMsg : msg
    , discardMsg : msg
    , fromFiltersMsg : Filters.Msg -> msg
    , fromSortersMsg : Sorters.Msg -> msg
    , index : Int
    , label : String
    , isOpen : Bool
    }


header :
    RenderConfig
    -> Filter msg item
    -> Sorters.ColumnStatus item
    -> Config msg
    -> Element msg
header renderConfig filter sorting config =
    let
        clearMsg =
            config.fromFiltersMsg <| Filters.Clear config.index
    in
    if config.isOpen then
        let
            applyMsg =
                config.fromFiltersMsg <| Filters.Apply config.index

            sortMsg =
                Sorters.SetSorting config.index >> config.fromSortersMsg

            terms =
                RenderConfig.localeTerms renderConfig

            applyButton =
                terms.filters.apply
                    |> Button.fromLabel
                    |> Button.cmd applyMsg Button.primary

            disabledApplyButton =
                terms.filters.apply
                    |> Button.fromLabel
                    |> Button.disabled

            clearButton =
                terms.filters.clear
                    |> Button.fromLabel
                    |> Button.cmd clearMsg Button.danger

            buttons =
                case ( Filter.isApplied filter, Filter.isEdited filter ) of
                    ( False, False ) ->
                        [ disabledApplyButton ]

                    ( False, True ) ->
                        [ applyButton ]

                    ( True, False ) ->
                        [ clearButton ]

                    ( True, True ) ->
                        [ applyButton, clearButton ]

            filterRender renderer editable =
                FilterV2.body
                    config.label
                    config.discardMsg
                    |> FilterV2.bodyWithSize FilterV2.sizeExtraSmall
                    |> FilterV2.bodyWithRows
                        (renderer renderConfig applyMsg config editable)
                    |> FilterV2.bodyWithSorting
                        { preview =
                            Maybe.andThen
                                (Tuple.second
                                    >> Sorter.preview
                                    >> Maybe.map (\( s, l ) -> { smaller = s, larger = l })
                                )
                                sorting
                        , ascendingSortMsg = sortMsg SortAscending
                        , descendingSortMsg = sortMsg SortDescending
                        , clearSortMsg = config.fromSortersMsg Sorters.ClearSorting
                        , applied = Maybe.andThen Tuple.first sorting
                        }
                    |> FilterV2.bodyWithButtons buttons
                    |> FilterV2.bodyWithWidth (fill |> minimum 180)
                    |> FilterV2.bodyToElement renderConfig
        in
        case filter of
            Filter.SingleTextFilter { editable } ->
                filterRender singleTextFilterRender editable

            Filter.MultiTextFilter { editable } ->
                filterRender multiTextFilterRender editable

            Filter.SingleDateFilter { editable } ->
                filterRender singleDateFilterRender editable

            Filter.RangeDateFilter { editable } ->
                filterRender rangeDateFilterRender editable

            Filter.PeriodDateFilter { editable } ->
                filterRender periodDateFilterRender editable

            Filter.SelectFilter list { editable } ->
                filterRender (selectFilterRender list) editable

    else
        FilterV2.header
            config.label
            config.openMsg
            |> FilterV2.headerWithSize FilterV2.sizeExtraSmall
            |> FilterV2.headerWithSorting
                (Maybe.andThen Tuple.first sorting)
            |> FilterV2.headerWithApplied
                (Maybe.map
                    (\i -> { preview = String.fromInt i, clearMsg = clearMsg })
                    (Filter.appliedLength filter)
                )
            |> FilterV2.headerToElement renderConfig



-- Selectable reuses filter background


headerSelectToggle : RenderConfig -> msg -> Element msg
headerSelectToggle renderConfig toggleMsg =
    let
        headerAttrs =
            Element.onIndividualClick toggleMsg
                :: Primitives.roundedBorders contextSize
                :: Element.width Element.fill
                :: Element.padding ((34 - 16) // 2)
                :: Element.spacing 8
                :: Element.pointer
                :: Background.color Colors.gray200
                :: Element.mouseOver [ Background.color Colors.navyBlue200 ]
                :: Element.colorTransition 100
                ++ ARIA.toElementAttributes ARIA.roleButton
    in
    (localeTerms renderConfig |> .tables |> .selectAll)
        |> Icon.check
        |> Icon.withSize contextSize
        |> Icon.renderElement renderConfig
        |> Element.el headerAttrs
        |> Element.el
            [ Element.width (px 32) ]



-- Standard size used for headers


contextSize : Size
contextSize =
    Size.ExtraSmall



-- Specifics


singleTextFilterRender :
    RenderConfig
    -> msg
    -> Config msg
    -> Filter.Editable String
    -> List (Element msg)
singleTextFilterRender renderConfig applyMsg { fromFiltersMsg, index, label } editable =
    let
        editMsg str =
            fromFiltersMsg <| Filters.EditSingleText { column = index, value = str }
    in
    editable
        |> Filter.editableWithDefault ""
        |> TextField.singlelineText editMsg label
        |> TextField.withSize contextSize
        |> TextField.withWidth TextField.widthFull
        |> TextField.withOnEnterPressed applyMsg
        |> TextField.renderElement renderConfig
        |> List.singleton


multiTextFilterRender :
    RenderConfig
    -> msg
    -> Config msg
    -> Filter.Editable (Array String)
    -> List (Element msg)
multiTextFilterRender renderConfig applyMsg { fromFiltersMsg, index, label } editableArr =
    let
        editMsg subIndex str =
            fromFiltersMsg <| Filters.EditMultiText { column = index, field = subIndex, value = str }

        rowField subIndex line =
            line
                |> TextField.singlelineText (editMsg subIndex) label
                |> TextField.withSize contextSize
                |> TextField.withWidth TextField.widthFull
                |> TextField.withOnEnterPressed applyMsg
                |> TextField.renderElement renderConfig
    in
    editableArr
        |> Filter.editableWithDefault Array.empty
        |> Array.push ""
        |> Array.indexedMap rowField
        |> Array.toList


selectFilterRender :
    List String
    -> RenderConfig
    -> msg
    -> Config msg
    -> Filter.Editable Int
    -> List (Element msg)
selectFilterRender list renderConfig _ { fromFiltersMsg, index } { current, applied } =
    Radio.group
        { label = renderConfig |> localeTerms >> .filters >> .select >> .description
        , onSelectMsg = \_ subIndex -> fromFiltersMsg <| Filters.EditSelect { column = index, value = subIndex }
        , idPrefix = "table-select-filter"
        }
        |> Radio.withSelected (maybeNotThen applied current)
        |> Radio.withButtons (List.indexedMap Radio.button list)
        |> Radio.withWidth Radio.widthFull
        |> Radio.withSize Radio.sizeSM
        |> Radio.renderElement renderConfig
        |> List.singleton


singleDateFilterRender :
    RenderConfig
    -> msg
    -> Config msg
    -> Filter.Editable DateInput
    -> List (Element msg)
singleDateFilterRender renderConfig applyMsg { fromFiltersMsg, index, label } editable =
    let
        editMsg str =
            fromFiltersMsg <| Filters.EditSingleDate { column = index, value = str }

        datePlaceholder =
            renderConfig |> localeTerms >> .filters >> .dateFormat
    in
    editable
        |> Filter.editableWithDefault (DateInvalid "")
        |> dateInput renderConfig applyMsg editMsg datePlaceholder label
        |> TextField.renderElement renderConfig
        |> List.singleton


rangeDateFilterRender :
    RenderConfig
    -> msg
    -> Config msg
    -> Filter.Editable RangeDate
    -> List (Element msg)
rangeDateFilterRender renderConfig applyMsg { fromFiltersMsg, index, label } editable =
    let
        editFromMsg str =
            fromFiltersMsg <| Filters.EditRangeFromDate { column = index, value = str }

        editToMsg str =
            fromFiltersMsg <| Filters.EditRangeToDate { column = index, value = str }

        filtersTerms =
            renderConfig |> localeTerms >> .filters

        current =
            editable
                |> Filter.editableWithDefault
                    { from = DateInvalid "", to = DateInvalid "" }

        fromPlaceholder =
            filtersTerms.range.from { date = filtersTerms.dateFormat }

        toPlaceholder =
            filtersTerms.range.to { date = filtersTerms.dateFormat }
    in
    [ current.from
        |> dateInput renderConfig applyMsg editFromMsg fromPlaceholder label
        |> TextField.renderElement renderConfig
    , current.to
        |> dateInput renderConfig applyMsg editToMsg toPlaceholder label
        |> TextField.renderElement renderConfig
    ]


periodDateFilterRender :
    RenderConfig
    -> msg
    -> Config msg
    -> Filter.Editable PeriodDate
    -> List (Element msg)
periodDateFilterRender renderConfig applyMsg { fromFiltersMsg, index, label } editable =
    let
        editDateMsg str =
            fromFiltersMsg <| Filters.EditPeriodDate { column = index, value = str }

        editComparisonMsg _ value =
            fromFiltersMsg <| Filters.EditPeriodComparison { column = index, value = value }

        current =
            editable
                |> Filter.editableWithDefault { date = DateInvalid "", comparison = On }

        filtersTerms =
            renderConfig |> localeTerms >> .filters

        options =
            [ Radio.button On filtersTerms.period.on
            , Radio.button Before filtersTerms.period.before
            , Radio.button After filtersTerms.period.after
            ]
    in
    [ current.date
        |> dateInput renderConfig applyMsg editDateMsg filtersTerms.dateFormat label
        |> TextField.renderElement renderConfig
    , Radio.group
        { label = filtersTerms.period.description
        , onSelectMsg = editComparisonMsg
        , idPrefix = "table-date-period-filter"
        }
        |> Radio.withSelected (Just current.comparison)
        |> Radio.withButtons options
        |> Radio.withWidth Radio.widthFull
        |> Radio.withSize Radio.sizeSM
        |> Radio.renderElement renderConfig
    ]



-- Internal


dateInput : RenderConfig -> msg -> (String -> msg) -> String -> String -> DateInput -> TextField msg
dateInput cfg applyMsg editMsg placeholder label current =
    current
        |> DateInput.toTextField cfg Filter.dateSeparator editMsg label
        |> TextField.withPlaceholder placeholder
        |> TextField.withSize contextSize
        |> TextField.withWidth TextField.widthFull
        |> TextField.withOnEnterPressed applyMsg
