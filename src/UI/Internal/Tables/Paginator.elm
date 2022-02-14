module UI.Internal.Tables.Paginator exposing
    ( Paginator
    , State
    , basic
    , init
    , renderElement
    , toggleMenu
    , withAmountByPage
    , withIndex
    , withPageAmountOptions
    )

import Element exposing (Element)
import Element.Border as Border
import Element.Keyed as Keyed
import UI.Button as Button
import UI.Icon as Icon
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Menu as Menu exposing (MenuItem)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text
import UI.Utils.Element exposing (zeroPadding)


type Paginator msg
    = Paginator (Properties msg) Options


type alias Properties msg =
    { onChangeIndex : Int -> msg
    , onChangeAmountByPage : Int -> msg
    , onToggleMenu : msg
    , totalAmount : Int
    , state : State
    }


type alias Options =
    { index : Int
    , amountByPage : Int
    , pageAmountOptions : List Int
    }


basic :
    { onChangeIndex : Int -> msg
    , onChangeAmountByPage : Int -> msg
    , onToggleMenu : msg
    , totalAmount : Int
    , state : State
    }
    -> Paginator msg
basic prop =
    Paginator prop
        { index = 1
        , amountByPage = 15
        , pageAmountOptions = [ 25, 50, 100 ]
        }


type State
    = State StateModel


type alias StateModel =
    { menu : Bool }


init : State
init =
    State
        { menu = False }


toggleMenu : State -> State
toggleMenu (State state) =
    State { state | menu = not state.menu }


withIndex : Int -> Paginator msg -> Paginator msg
withIndex item (Paginator prop opt) =
    Paginator prop { opt | index = item }


withAmountByPage : Int -> Paginator msg -> Paginator msg
withAmountByPage pageAmount (Paginator prop opt) =
    Paginator prop { opt | amountByPage = pageAmount }


withPageAmountOptions : List Int -> Paginator msg -> Paginator msg
withPageAmountOptions pageAmountOptions (Paginator prop opt) =
    Paginator prop { opt | pageAmountOptions = pageAmountOptions }


renderElement : RenderConfig -> Paginator msg -> Element msg
renderElement renderConfig paginator =
    Keyed.row
        [ Element.width Element.fill
        , Element.paddingXY 8 4
        , Border.widthEach { zeroPadding | top = 1 }
        , Palette.gray200
            |> Palette.toElementColor
            |> Border.color
        ]
        [ rangeInfo renderConfig paginator
        , renderPaginator renderConfig paginator
        , pageAmountSelector renderConfig paginator
        ]


rangeInfo : RenderConfig -> Paginator msg -> ( String, Element msg )
rangeInfo renderConfig (Paginator { totalAmount } { index, amountByPage }) =
    let
        paginatorFormat =
            renderConfig
                |> localeTerms
                |> .paginator
                |> .tableFormat
    in
    paginatorFormat
        { first = String.fromInt <| index + 1
        , last = String.fromInt (min (index + amountByPage) totalAmount)
        , total = String.fromInt totalAmount
        }
        |> Text.body2
        |> Text.withOverflow Text.wrap
        |> Text.renderElement renderConfig
        |> Element.el [ Element.width <| Element.minimum 96 Element.shrink ]
        |> Tuple.pair "range-info"


renderPaginator : RenderConfig -> Paginator msg -> ( String, Element msg )
renderPaginator renderConfig (Paginator { onChangeIndex, totalAmount } { index, amountByPage }) =
    let
        currentPage =
            ceiling <| toFloat index / toFloat amountByPage

        totalPages =
            floor <| toFloat totalAmount / toFloat amountByPage

        isFistPage =
            currentPage <= 0

        isLastPage =
            currentPage >= totalPages
    in
    Keyed.row
        [ Element.spacingXY 8 0
        , Element.centerX
        ]
        [ firstPageButton renderConfig
            (onChangeIndex 0)
            isFistPage
        , previousButton renderConfig
            (onChangeIndex <| (currentPage - 1) * amountByPage)
            isFistPage
        , renderPages renderConfig onChangeIndex currentPage totalPages amountByPage
        , nextButton
            renderConfig
            (onChangeIndex <| (currentPage + 1) * amountByPage)
            isLastPage
        , lastPageButton renderConfig
            (onChangeIndex <| totalPages * amountByPage)
            isLastPage
        ]
        |> Tuple.pair "paginator"


firstPageButton : RenderConfig -> msg -> Bool -> ( String, Element msg )
firstPageButton renderConfig msg isDisabled =
    renderConfig
        |> localeTerms
        |> .paginator
        |> .first
        |> Button.fromLabel
        |> Button.cmd msg Button.light
        |> Button.withSize Size.small
        |> Button.withDisabledIf isDisabled
        |> Button.renderElement renderConfig
        |> Tuple.pair "first"


previousButton : RenderConfig -> msg -> Bool -> ( String, Element msg )
previousButton renderConfig msg isDisabled =
    Button.fromIcon
        (renderConfig
            |> localeTerms
            |> .paginator
            |> .previous
            |> Icon.previousContent
        )
        |> Button.cmd msg Button.light
        |> Button.withSize Size.small
        |> Button.withDisabledIf isDisabled
        |> Button.renderElement renderConfig
        |> Tuple.pair "previous"


renderPages : RenderConfig -> (Int -> msg) -> Int -> Int -> Int -> ( String, Element msg )
renderPages renderConfig onChangeIndex currentPage totalPages amountByPage =
    let
        ( leftPageRange, rightPageRange ) =
            pageRanges totalPages 4 currentPage
    in
    Keyed.row
        [ Element.spacingXY 2 0
        , Element.paddingXY 8 0
        ]
        [ leftPageRange
            |> List.map (indexButton renderConfig onChangeIndex amountByPage False)
            |> Keyed.row []
            |> Tuple.pair "behind"
        , indexButton renderConfig onChangeIndex amountByPage True currentPage
        , rightPageRange
            |> List.map (indexButton renderConfig onChangeIndex amountByPage False)
            |> Keyed.row []
            |> Tuple.pair "after"
        ]
        |> Tuple.pair "indexer"


pageRanges : Int -> Int -> Int -> ( List Int, List Int )
pageRanges total range page =
    let
        from =
            page
                - range
                - max 0 ((page + range) - total)
                |> max 0

        to =
            page
                + range
                + abs (min 0 (page - 1 - range))
                |> min total
    in
    ( if page > 0 then
        List.range (max 0 from) (page - 1)

      else
        []
    , if page < total then
        List.range (page + 1) (min total to)

      else
        []
    )


nextButton : RenderConfig -> msg -> Bool -> ( String, Element msg )
nextButton renderConfig msg isDisabled =
    Button.fromIcon
        (renderConfig
            |> localeTerms
            |> .paginator
            |> .next
            |> Icon.nextContent
        )
        |> Button.cmd msg Button.light
        |> Button.withSize Size.small
        |> Button.withDisabledIf isDisabled
        |> Button.renderElement renderConfig
        |> Tuple.pair "next"


lastPageButton : RenderConfig -> msg -> Bool -> ( String, Element msg )
lastPageButton renderConfig msg isDisabled =
    renderConfig
        |> localeTerms
        |> .paginator
        |> .last
        |> Button.fromLabel
        |> Button.cmd msg Button.light
        |> Button.withSize Size.small
        |> Button.withDisabledIf isDisabled
        |> Button.renderElement renderConfig
        |> Tuple.pair "last-page"


indexButton : RenderConfig -> (Int -> msg) -> Int -> Bool -> Int -> ( String, Element msg )
indexButton renderConfig onChangeIndex amountByPage isCurentPage item =
    let
        label =
            String.fromInt <| item + 1
    in
    Button.fromLabel label
        |> Button.cmd (onChangeIndex <| item * amountByPage)
            (indexButtonStyle isCurentPage)
        |> Button.withSize Size.small
        |> Button.renderElement renderConfig
        |> Tuple.pair label


indexButtonStyle : Bool -> Button.ButtonStyle
indexButtonStyle isCurrentPage =
    if isCurrentPage then
        Button.primary

    else
        Button.light


pageAmountSelector : RenderConfig -> Paginator msg -> ( String, Element msg )
pageAmountSelector renderConfig paginator =
    Keyed.row
        [ Element.width <| Element.px 150
        , Element.spacingXY 8 0
        ]
        [ Text.body2 "Rows/Page"
            |> Text.renderElement renderConfig
            |> Tuple.pair "rows-page"
        , paginator
            |> amountMenu renderConfig
            |> Tuple.pair "amount-menu"
        ]
        |> Tuple.pair "page-amount-selector"


amountMenu : RenderConfig -> Paginator msg -> Element msg
amountMenu renderConfig (Paginator prop opt) =
    Button.fromLabeledOnLeftIcon (Icon.chevronDown <| String.fromInt opt.amountByPage)
        |> Button.cmd prop.onToggleMenu Button.light
        |> Button.withSize Size.small
        |> Menu.menu prop.onToggleMenu
            (List.map (amountMenuItem prop.onChangeAmountByPage) opt.pageAmountOptions)
        |> Menu.setVisible (menuIsVisible prop.state)
        |> Menu.renderElement renderConfig


amountMenuItem : (Int -> msg) -> Int -> MenuItem msg
amountMenuItem onChangePageAmount amountOption =
    amountOption
        |> String.fromInt
        |> Menu.item (onChangePageAmount amountOption) Nothing


menuIsVisible : State -> Bool
menuIsVisible (State { menu }) =
    menu
