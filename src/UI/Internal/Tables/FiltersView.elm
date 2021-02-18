module UI.Internal.Tables.FiltersView exposing (Config, header, headerSelectToggle)

-- WARNING: Don't use any other Size.* beyond "contextSize"

import Array exposing (Array)
import Element exposing (Attribute, Element, fill, px, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import UI.Button as Button
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Basics exposing (maybeNotThen, prependIf)
import UI.Internal.Colors as Colors
import UI.Internal.DateInput as DateInput exposing (DateInput(..), PeriodComparison(..), PeriodDate, RangeDate)
import UI.Internal.Primitives as Primitives
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Internal.Size as Size exposing (Size)
import UI.Internal.Tables.Filters as Filters
import UI.Internal.Tables.Sorters as Sorters exposing (SortingDirection(..))
import UI.Internal.Text as Text
import UI.Internal.Utils.Element exposing (overlay, shrinkButClip, tuplesToStyles, zIndex)
import UI.Palette as Palette exposing (brightnessDarkest, brightnessMiddle, tonePrimary)
import UI.Radio as Radio
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text
import UI.TextField as TextField exposing (TextField)
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element exposing (zeroPadding)


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
    -> Filters.Filter msg item
    -> Sorters.ColumnStatus
    -> Config msg
    -> Element msg
header renderConfig filter sorting config =
    let
        clearMsg =
            config.fromFiltersMsg <| Filters.Clear config.index

        applyMsg =
            config.fromFiltersMsg <| Filters.Apply config.index

        filterRender renderer editable =
            renderer renderConfig applyMsg config editable
                |> dialog renderConfig config filter sorting clearMsg applyMsg
    in
    if config.isOpen then
        case filter of
            Filters.SingleTextFilter { editable } ->
                filterRender singleTextFilterRender editable

            Filters.MultiTextFilter { editable } ->
                filterRender multiTextFilterRender editable

            Filters.SingleDateFilter { editable } ->
                filterRender singleDateFilterRender editable

            Filters.RangeDateFilter { editable } ->
                filterRender rangeDateFilterRender editable

            Filters.PeriodDateFilter { editable } ->
                filterRender periodDateFilterRender editable

            Filters.SelectFilter list { editable } ->
                selectFilterRender renderConfig config list editable
                    |> dialog renderConfig config filter sorting clearMsg applyMsg

    else if Filters.isApplied filter then
        headerApplied renderConfig
            config.openMsg
            clearMsg
            (renderConfig |> localeTerms >> .filters >> .clear)
            config.label
            sorting

    else
        headerNormal renderConfig
            config.openMsg
            config.label
            sorting



-- Selectable reuses filter background


headerSelectToggle : RenderConfig -> msg -> Element msg
headerSelectToggle renderConfig toggleMsg =
    Icon.check ""
        |> Icon.withSize contextSize
        |> Icon.renderElement renderConfig
        |> Element.el
            (Element.onIndividualClick toggleMsg
                :: headerAttrs False
            )
        |> Element.el
            [ Element.width (px 32) ]



-- Mostly ripped from Button with Size.Small and WidthFull


headerNormal : RenderConfig -> msg -> String -> Sorters.ColumnStatus -> Element msg
headerNormal renderConfig openMsg label sorting =
    -- Button.light
    Element.row (Element.onIndividualClick openMsg :: headerAttrs False)
        [ headerText renderConfig False label
        , headerSorting renderConfig False sorting
        , Icon.filter label
            |> Icon.withSize contextSize
            |> Icon.withColor (headerColor False)
            |> Icon.renderElement renderConfig
            |> Element.el [ Element.alignRight ]
        ]


headerApplied : RenderConfig -> msg -> msg -> String -> String -> Sorters.ColumnStatus -> Element msg
headerApplied renderConfig openMsg clearMsg clearHint label sorting =
    -- Button.primary
    Element.row (Element.onIndividualClick openMsg :: headerAttrs True)
        [ headerText renderConfig True label
        , headerSorting renderConfig True sorting
        , Button.fromIcon (Icon.close clearHint)
            |> Button.cmd clearMsg Button.primary
            |> Button.withSize contextSize
            |> Button.renderElement renderConfig
            |> Element.el [ Element.alignRight ]
        ]


headerText : RenderConfig -> Bool -> String -> Element msg
headerText renderConfig isApplied label =
    label
        |> Text.ellipsizedText renderConfig Text.SizeCaption
        |> Element.el
            (Font.size textSize
                :: Font.semiBold
                :: Font.color (Palette.toElementColor <| headerColor isApplied)
                :: shrinkButClip
            )


headerColor : Bool -> Palette.Color
headerColor isApplied =
    if isApplied then
        Palette.color
            tonePrimary
            brightnessMiddle
            |> Palette.setContrasting True

    else
        Palette.color
            tonePrimary
            brightnessDarkest


headerPadX : Int
headerPadX =
    (36 - 16) // 2


headerAttrs : Bool -> List (Attribute msg)
headerAttrs isApplied =
    let
        baseHeight =
            if isApplied then
                24

            else
                16

        paddingXY =
            Element.paddingXY
                headerPadX
                ((36 - baseHeight) // 2)

        workingTheme =
            if isApplied then
                [ Background.color Colors.primary.middle ]

            else
                Background.color
                    Colors.gray.lightest
                    :: Element.mouseOver [ Background.color Colors.primary.lightest ]
                    :: Element.colorTransition 100
    in
    Primitives.roundedBorders
        contextSize
        :: Element.width Element.fill
        :: paddingXY
        :: Element.spacing 8
        :: Element.pointer
        :: (ARIA.toElementAttributes ARIA.roleButton
                ++ workingTheme
           )


headerSorting : RenderConfig -> Bool -> Sorters.ColumnStatus -> Element msg
headerSorting renderConfig isFilterApplied status =
    case Maybe.andThen identity status of
        Just direction ->
            sortingDirectionToIcon renderConfig direction
                |> Icon.withCustomSize 12
                |> Icon.withColor (headerColor isFilterApplied)
                |> Icon.renderElement renderConfig
                |> Element.el [ Element.centerY ]

        Nothing ->
            Element.none



-- Standard size used for headers


textSize : Int
textSize =
    12


contextSize : Size
contextSize =
    Size.ExtraSmall



-- Editing


filterEditingButton : RenderConfig -> msg -> msg -> Bool -> Bool -> Element msg
filterEditingButton renderConfig applyMsg clearMsg applied current =
    let
        filtersTerms =
            renderConfig |> localeTerms >> .filters

        clearBtn =
            Button.fromLabel filtersTerms.clear
                |> Button.cmd clearMsg Button.danger
                |> Button.withSize contextSize

        applyBtn =
            Button.fromLabel filtersTerms.apply
                |> Button.cmd applyMsg Button.primary
                |> Button.withSize contextSize

        disabledBtn =
            applyBtn |> Button.withDisabledIf True
    in
    case ( applied, current ) of
        ( _, True ) ->
            [ applyBtn, clearBtn ]
                |> List.map (Button.renderElement renderConfig)
                |> Element.row [ Element.spacing 8 ]

        ( True, False ) ->
            Button.renderElement renderConfig clearBtn

        ( False, False ) ->
            Button.renderElement renderConfig disabledBtn


dialogHeader : RenderConfig -> msg -> String -> Element msg
dialogHeader renderConfig discardMsg label =
    Element.row
        [ Element.paddingEach { top = 10, left = headerPadX, right = 10, bottom = 9 }
        , Element.width fill
        , Border.color Colors.gray.lighter
        , Border.widthEach { zeroPadding | bottom = 1 }
        ]
        [ filteredHeaderLabel label
        , dialogClose renderConfig discardMsg
        ]


filteredHeaderLabel : String -> Element msg
filteredHeaderLabel label =
    label
        |> Element.text
        |> Element.el [ Font.bold, Font.size textSize ]


dialogClose : RenderConfig -> msg -> Element msg
dialogClose renderConfig message =
    (renderConfig |> localeTerms >> .filters >> .close)
        |> Icon.close
        |> Icon.withColor (Palette.color Palette.toneGray Palette.brightnessLight)
        |> Icon.withSize contextSize
        |> Icon.renderElement renderConfig
        |> Element.el
            (ARIA.toElementAttributes ARIA.roleButton
                ++ [ Events.onClick message
                   , Element.pointer
                   , Element.centerY
                   , Element.height shrink
                   , Element.alignRight
                   ]
            )


dialog :
    RenderConfig
    -> Config msg
    -> Filters.Filter msg item
    -> Sorters.ColumnStatus
    -> msg
    -> msg
    -> Element msg
    -> Element msg
dialog renderConfig config filter sorter clearMsg applyMsg content =
    let
        applied =
            Filters.isApplied filter

        current =
            Filters.isEdited filter
    in
    overlay config.discardMsg <|
        Element.column
            [ zIndex 9
            , Element.alignTop
            , Colors.mainBackground
            , Primitives.defaultRoundedBorders
            , tuplesToStyles ( "min-width", "100%" )
            , tuplesToStyles ( "width", "min-content" )
            ]
            [ dialogHeader renderConfig config.discardMsg config.label
            , sortingView renderConfig config sorter
            , Element.column
                [ Element.width fill
                , Element.spacing 12
                ]
                [ content
                , filterEditingButton renderConfig applyMsg clearMsg applied current
                    |> internalPaddingBox
                ]
            ]


sortingView : RenderConfig -> Config msg -> Sorters.ColumnStatus -> Element msg
sortingView renderConfig config sorter =
    case sorter of
        Just _ ->
            Element.column [ Element.width fill ]
                [ sortAs renderConfig config SortIncreasing sorter
                , sortAs renderConfig config SortDecreasing sorter
                ]

        Nothing ->
            -- Unsortable
            Element.none


sortAs : RenderConfig -> Config msg -> SortingDirection -> Sorters.ColumnStatus -> Element msg
sortAs renderConfig { fromSortersMsg, index } direction current =
    let
        terms =
            localeTerms renderConfig

        ( content, icon ) =
            case direction of
                SortIncreasing ->
                    ( terms.tables.sorting.increase, Icon.sortIncreasing )

                SortDecreasing ->
                    ( terms.tables.sorting.decrease, Icon.sortDecreasing )

        selected =
            current == Just (Just direction)

        msg =
            if selected then
                Sorters.ClearSorting

            else
                Sorters.SetSorting index direction
    in
    Element.row
        (Events.onClick (fromSortersMsg msg)
            :: Element.pointer
            :: Element.width fill
            :: Element.paddingEach { top = 4, left = 12, bottom = 4, right = 8 }
            :: Border.color Colors.gray.lighter
            :: Border.widthEach { zeroPadding | bottom = 1 }
            :: Element.mouseOver [ Background.color Colors.gray.lightest ]
            :: ARIA.toElementAttributes ARIA.roleButton
            |> prependIf selected (Background.color <| Colors.primary.lightest)
        )
        [ Text.caption content
            |> Text.withColor (Palette.color tonePrimary brightnessMiddle)
            |> Text.renderElement renderConfig
        , icon content
            |> Icon.withColor (Palette.color tonePrimary brightnessMiddle)
            |> Icon.withSize Size.extraSmall
            |> Icon.renderElement renderConfig
        ]



-- Specifics


singleTextFilterRender :
    RenderConfig
    -> msg
    -> Config msg
    -> Filters.Editable String
    -> Element msg
singleTextFilterRender renderConfig applyMsg { fromFiltersMsg, index, label } editable =
    let
        editMsg str =
            fromFiltersMsg <| Filters.EditSingleText { column = index, value = str }
    in
    editable
        |> Filters.editableWithDefault ""
        |> TextField.singlelineText editMsg label
        |> TextField.withSize contextSize
        |> TextField.withWidth TextField.widthFull
        |> TextField.withOnEnterPressed applyMsg
        |> TextField.renderElement renderConfig
        |> internalPaddingBox


multiTextFilterRender :
    RenderConfig
    -> msg
    -> Config msg
    -> Filters.Editable (Array String)
    -> Element msg
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
        |> Filters.editableWithDefault Array.empty
        |> Array.push ""
        |> Array.indexedMap rowField
        |> Array.toList
        |> Element.column [ Element.width fill, Element.spacing 8, internalPadding ]


selectFilterRender :
    RenderConfig
    -> Config msg
    -> List String
    -> Filters.Editable Int
    -> Element msg
selectFilterRender renderConfig { fromFiltersMsg, index } list { current, applied } =
    Radio.group
        (renderConfig |> localeTerms >> .filters >> .select >> .description)
        (\subIndex -> fromFiltersMsg <| Filters.EditSelect { column = index, value = subIndex })
        |> Radio.withSelected (maybeNotThen applied current)
        |> Radio.withButtons (List.indexedMap Radio.button list)
        |> Radio.withWidth Radio.widthFull
        |> Radio.renderElement renderConfig


singleDateFilterRender :
    RenderConfig
    -> msg
    -> Config msg
    -> Filters.Editable DateInput
    -> Element msg
singleDateFilterRender renderConfig applyMsg { fromFiltersMsg, index, label } editable =
    let
        editMsg str =
            fromFiltersMsg <| Filters.EditSingleDate { column = index, value = str }

        datePlaceholder =
            renderConfig |> localeTerms >> .filters >> .dateFormat
    in
    editable
        |> Filters.editableWithDefault (DateInvalid "")
        |> dateInput renderConfig applyMsg editMsg datePlaceholder label
        |> TextField.renderElement renderConfig
        |> internalPaddingBox


rangeDateFilterRender :
    RenderConfig
    -> msg
    -> Config msg
    -> Filters.Editable RangeDate
    -> Element msg
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
                |> Filters.editableWithDefault
                    { from = DateInvalid "", to = DateInvalid "" }

        fromPlaceholder =
            filtersTerms.range.from { date = filtersTerms.dateFormat }

        toPlaceholder =
            filtersTerms.range.to { date = filtersTerms.dateFormat }
    in
    Element.column
        [ Element.width fill
        , Element.spacing 8
        , internalPadding
        ]
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
    -> Filters.Editable PeriodDate
    -> Element msg
periodDateFilterRender renderConfig applyMsg { fromFiltersMsg, index, label } editable =
    let
        editDateMsg str =
            fromFiltersMsg <| Filters.EditPeriodDate { column = index, value = str }

        editComparisonMsg value =
            fromFiltersMsg <| Filters.EditPeriodComparison { column = index, value = value }

        current =
            editable
                |> Filters.editableWithDefault { date = DateInvalid "", comparison = On }

        filtersTerms =
            renderConfig |> localeTerms >> .filters

        options =
            [ Radio.button On filtersTerms.period.on
            , Radio.button Before filtersTerms.period.before
            , Radio.button After filtersTerms.period.after
            ]
    in
    Element.column
        [ Element.width fill
        ]
        [ current.date
            |> dateInput renderConfig applyMsg editDateMsg filtersTerms.dateFormat label
            |> TextField.renderElement renderConfig
            |> internalPaddingBox
        , Radio.group
            filtersTerms.period.description
            editComparisonMsg
            |> Radio.withSelected (Just current.comparison)
            |> Radio.withButtons options
            |> Radio.withWidth Radio.widthFull
            |> Radio.renderElement renderConfig
        ]



-- Internal


dateInput : RenderConfig -> msg -> (String -> msg) -> String -> String -> DateInput -> TextField msg
dateInput cfg applyMsg editMsg placeholder label current =
    current
        |> DateInput.toTextField cfg Filters.dateSeparator editMsg label
        |> TextField.withPlaceholder placeholder
        |> TextField.withSize contextSize
        |> TextField.withWidth TextField.widthFull
        |> TextField.withOnEnterPressed applyMsg


internalPadding : Attribute msg
internalPadding =
    Element.paddingXY 12 8


internalPaddingBox : Element msg -> Element msg
internalPaddingBox child =
    Element.el [ internalPadding ] child


sortingDirectionToIcon : RenderConfig -> SortingDirection -> Icon
sortingDirectionToIcon _ direction =
    case direction of
        SortIncreasing ->
            Icon.sortIncreasing ""

        SortDecreasing ->
            Icon.sortDecreasing ""
