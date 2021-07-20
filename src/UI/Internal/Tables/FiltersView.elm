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
import UI.Internal.Filter.Msg as Filter
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
    FilterV2.defaultFilter
        { openMsg = config.openMsg
        , closeMsg = config.discardMsg
        , editMsg = config.fromFiltersMsg << Filters.FilterMsg config.index
        , label = config.label
        , isOpen = config.isOpen
        }
        filter
        sorting
        |> FilterV2.renderElement renderConfig



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


multiTextFilterRender :
    RenderConfig
    -> msg
    -> Config msg
    -> Filter.Editable (Array String)
    -> List (Element msg)
multiTextFilterRender renderConfig applyMsg { fromFiltersMsg, index, label } editableArr =
    let
        editMsg subIndex str =
            fromFiltersMsg <| Filters.FilterMsg index <| Filter.EditMultiText subIndex str

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
        , onSelectMsg = \_ optionIndex -> fromFiltersMsg <| Filters.FilterMsg index <| Filter.EditSelect optionIndex
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
            fromFiltersMsg <| Filters.FilterMsg index <| Filter.EditSingleDate str

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
            fromFiltersMsg <| Filters.FilterMsg index <| Filter.EditRangeFromDate str

        editToMsg str =
            fromFiltersMsg <| Filters.FilterMsg index <| Filter.EditRangeToDate str

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
            fromFiltersMsg <| Filters.FilterMsg index <| Filter.EditPeriodDate str

        editComparisonMsg _ value =
            fromFiltersMsg <| Filters.FilterMsg index <| Filter.EditPeriodComparison value

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
