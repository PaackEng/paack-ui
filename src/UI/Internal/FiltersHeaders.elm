module UI.Internal.FiltersHeaders exposing (Config, header)

import Array exposing (Array)
import Element exposing (Attribute, Element, fill, minimum, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html.Attributes as HtmlAttrs
import UI.Button as Button
import UI.Checkbox exposing (radioButton)
import UI.Icon as Icon
import UI.Internal.Basics exposing (maybeNotThen)
import UI.Internal.Filters as Filters
import UI.Internal.Human exposing (Date(..), PeriodComparison(..), PeriodDate, RangeDate, dateIfValid, dateToNumericString)
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Internal.Size as Size exposing (Size)
import UI.Palette as Palette
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
    , index : Int
    , label : String
    , isOpen : Bool
    }


header :
    RenderConfig
    -> Filters.Filter msg item
    -> Config msg
    -> Element msg
header renderConfig filter config =
    let
        clearMsg =
            config.fromFiltersMsg <| Filters.Clear config.index

        applyMsg =
            config.fromFiltersMsg <| Filters.Apply config.index

        filterRender renderer editable =
            renderer renderConfig applyMsg config editable
                |> dialog renderConfig config filter clearMsg applyMsg
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
                    |> dialog renderConfig config filter clearMsg applyMsg

    else if Filters.isApplied filter then
        headerApplied renderConfig
            config.openMsg
            clearMsg
            "Clear"
            config.label

    else
        headerNormal renderConfig
            config.openMsg
            config.label



-- Mostly ripped from Button with Size.Small and WidthFull


headerNormal : RenderConfig -> msg -> String -> Element msg
headerNormal renderConfig openMsg label =
    -- Button.light
    Element.row (Element.onIndividualClick openMsg :: headerAttrs False)
        [ Element.text label
        , Icon.filter label
            |> Icon.withSize size
            |> Icon.renderElement renderConfig
            |> Element.el [ Element.alignRight ]
        ]


headerApplied : RenderConfig -> msg -> msg -> String -> String -> Element msg
headerApplied renderConfig openMsg clearMsg clearHint label =
    -- Button.primary
    Element.row (Element.onIndividualClick openMsg :: headerAttrs True)
        [ Element.text label
        , Button.fromIcon (Icon.close clearHint)
            |> Button.cmd clearMsg Button.danger
            |> Button.withSize Size.ExtraSmall
            |> Button.renderElement renderConfig
            |> Element.el [ Element.alignRight ]
        ]


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
                ((36 - 16) // 2)
                ((36 - baseHeight) // 2)

        workingTheme =
            if isApplied then
                [ Background.color Palette.primary.middle
                , Palette.color
                    Palette.tonePrimary
                    Palette.brightnessMiddle
                    |> Palette.setContrasting True
                    |> Palette.toElementColor
                    |> Font.color
                ]

            else
                [ Background.color Palette.gray.lightest
                , Font.color Palette.primary.darkest
                ]
    in
    [ Primitives.roundedBorders size
    , Element.width Element.fill
    , paddingXY
    , Element.spacing 8
    , Font.size textSize
    , Font.semiBold
    , Element.pointer
    ]
        ++ (ARIA.toElementAttributes <| ARIA.roleButton)
        ++ workingTheme


textSize : Int
textSize =
    12


size : Size
size =
    Size.Small



-- Editing


overlayBackground : msg -> Element msg
overlayBackground onClickMsg =
    Element.el
        [ positionFixed -- Needs for starting at the top-left corner
        , zIndex 8
        , Palette.overlayBackground
        , Element.htmlAttribute <| HtmlAttrs.style "top" "0"
        , Element.htmlAttribute <| HtmlAttrs.style "left" "0"
        , Element.htmlAttribute <| HtmlAttrs.style "width" "100vw"
        , Element.htmlAttribute <| HtmlAttrs.style "height" "100vh"
        , Events.onClick onClickMsg
        ]
        Element.none


filterEditingButton : RenderConfig -> msg -> msg -> Bool -> Bool -> Element msg
filterEditingButton renderConfig applyMsg clearMsg applied current =
    let
        clearBtn =
            Button.fromLabel "Clear"
                |> Button.cmd clearMsg Button.danger
                |> Button.withSize Size.extraSmall

        applyBtn =
            Button.fromLabel "Apply"
                |> Button.cmd applyMsg Button.primary
                |> Button.withSize Size.extraSmall

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
        [ Element.paddingEach { top = 10, left = 12, right = 10, bottom = 7 }
        , Element.width fill
        , Border.color Palette.gray.lighter
        , Border.widthEach { zeroPadding | bottom = 1 }
        ]
        [ Text.overline label
            |> Text.renderElement renderConfig
        , Button.fromIcon (Icon.close "Close")
            |> Button.cmd discardMsg Button.clear
            |> Button.withSize Size.extraSmall
            |> Button.renderElement renderConfig
        ]


dialog :
    RenderConfig
    -> Config msg
    -> Filters.Filter msg item
    -> msg
    -> msg
    -> Element msg
    -> Element msg
dialog renderConfig config filter clearMsg applyMsg content =
    let
        applied =
            Filters.isApplied filter

        current =
            Filters.isEdited filter
    in
    Element.el
        [ Element.width fill
        , Element.height (shrink |> minimum 1)
        , Element.inFront <|
            Element.column
                [ Element.width fill
                , zIndex 9
                , Element.alignTop
                , Palette.mainBackground
                , Primitives.defaultRoundedBorders
                ]
                [ dialogHeader renderConfig config.discardMsg config.label
                , Element.column
                    [ Element.width fill
                    , Element.padding 12
                    , Element.spacing 20
                    ]
                    [ content
                    , filterEditingButton renderConfig applyMsg clearMsg applied current
                    ]
                ]
        ]
        (overlayBackground config.discardMsg)



-- Some good-old CSS


positionFixed : Attribute msg
positionFixed =
    Element.htmlAttribute <| HtmlAttrs.style "position" "fixed"


zIndex : Int -> Attribute msg
zIndex val =
    Element.htmlAttribute <| HtmlAttrs.style "z-index" (String.fromInt val)



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
        |> TextField.withSize Size.extraSmall
        |> TextField.withWidth TextField.widthFull
        |> TextField.withOnEnterPressed applyMsg
        |> TextField.renderElement renderConfig


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

        single subIndex line =
            line
                |> TextField.singlelineText (editMsg subIndex) label
                |> TextField.withSize Size.extraSmall
                |> TextField.withWidth TextField.widthFull
                |> TextField.withOnEnterPressed applyMsg
                |> TextField.renderElement renderConfig

        instanceArr =
            Filters.editableWithDefault Array.empty editableArr
    in
    instanceArr
        |> Array.push ""
        |> Array.indexedMap single
        |> Array.toList
        |> Element.column [ Element.width fill, Element.spacing 8 ]


selectFilterRender :
    RenderConfig
    -> Config msg
    -> List String
    -> Filters.Editable Int
    -> Element msg
selectFilterRender renderConfig { fromFiltersMsg, index } list { current, applied } =
    let
        editMsg subIndex =
            fromFiltersMsg <| Filters.EditSelect { column = index, value = subIndex }

        selected =
            maybeNotThen applied current

        single subIndex line =
            radioButton
                renderConfig
                (always <| editMsg subIndex)
                line
                (selected == Just subIndex)
    in
    list
        |> List.indexedMap single
        |> Element.column [ Element.spacing 8 ]


singleDateFilterRender :
    RenderConfig
    -> msg
    -> Config msg
    -> Filters.Editable Date
    -> Element msg
singleDateFilterRender renderConfig applyMsg { fromFiltersMsg, index, label } editable =
    let
        editMsg str =
            fromFiltersMsg <| Filters.EditSingleDate { column = index, value = str }

        current =
            editable
                |> Filters.editableWithDefault (DateInvalid "")

        correctInput =
            dateInput applyMsg editMsg "DD/MM/YYYY" label current
    in
    current
        |> validDateField correctInput
        |> TextField.renderElement renderConfig


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

        current =
            editable
                |> Filters.editableWithDefault
                    { from = DateInvalid "", to = DateInvalid "" }

        correctFromInput =
            dateInput applyMsg editFromMsg "From: DD/MM/YYYY" label current.from

        correctToInput =
            dateInput applyMsg editToMsg "To: DD/MM/YYYY" label current.to
    in
    Element.column
        [ Element.width fill
        , Element.spacing 8
        ]
        [ current.from
            |> validDateField correctFromInput
            |> TextField.renderElement renderConfig
        , current.to
            |> validDateField correctToInput
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

        correctInput =
            dateInput applyMsg editDateMsg "DD/MM/YYYY" label current.date
    in
    Element.column
        [ Element.width fill
        , Element.spacing 8
        ]
        [ current.date
            |> validDateField correctInput
            |> TextField.renderElement renderConfig
        , radioButton
            renderConfig
            (always <| editComparisonMsg On)
            "On"
            (current.comparison == On)
        , radioButton
            renderConfig
            (always <| editComparisonMsg Before)
            "Before"
            (current.comparison == Before)
        , radioButton
            renderConfig
            (always <| editComparisonMsg After)
            "After"
            (current.comparison == After)
        ]



-- Internal


dateInput : msg -> (String -> msg) -> String -> String -> Date -> TextField msg
dateInput applyMsg editMsg placeholder label current =
    current
        |> dateToNumericString
        |> TextField.singlelineText editMsg label
        |> TextField.withPlaceholder placeholder
        |> TextField.withSize Size.extraSmall
        |> TextField.withWidth TextField.widthFull
        |> TextField.withOnEnterPressed applyMsg


validDateField : TextField msg -> Date -> TextField msg
validDateField field date =
    dateIfValid field (TextField.withError "Invalid date format." field) date
