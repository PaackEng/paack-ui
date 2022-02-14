module UI.Internal.Tables.Paginator exposing
    ( Paginator, paginator
    , State, init, toggleMenu
    , withCurrentItem, withPageAmount, withPageAmountOptions
    , renderElement
    )

{-| `UI.Paginator` is a component for helping navigation in a large sample of elements.
It provides navigation buttons and current location information.

A paginator does not include the logic required for taking/dropping the source of elements, and neither does the rendering of these elements.
The following code applies the paginator to some simple list, and also applies paginating logic on it:

    -- TODO: UPDATE DOC
    Element.column
        [ Element.width fill
        , Element.height (px 600)
        ]
        [ model.options
            |> List.drop model.pageOffset
            |> List.take 20
            |> List.map itemView
            |> Element.column
                [ Element.width fill
                , Element.height fill
                ]
        , Paginator.nonNumeric
            { onForwardClicked = Msg.NextPage
            , onPreviousClicked = Msg.PreviousPage
            , totalAmount = List.length model.options
            , pageAmount = model.pageOffset
            }
            |> Paginator.withCurrentItem model.current
            |> Paginator.renderElement renderConfig
        ]


# Building

@docs Paginator, paginator


# State

@docs State, init, toggleMenu


# Options

@docs withCurrentItem, withPageAmount, withPageAmountOptions


# Rendering

@docs renderElement

-}

import Element exposing (Element)
import Element.Keyed as Keyed
import UI.Button as Button
import UI.Icon as Icon
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Menu as Menu exposing (MenuItem)
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text



--import UI.Utils.Element exposing (zeroPadding)


{-| The `Paginator msg` type is used for describing the component for later rendering.
-}
type Paginator msg
    = Paginator (Properties msg) Options


type alias Properties msg =
    { onChangeItem : Int -> msg
    , onChangePageAmount : Int -> msg
    , onToggleMenu : msg
    , totalAmount : Int
    , state : State
    }


type alias Options =
    { currentItem : Int
    , pageAmount : Int
    , amountOptions : List Int
    }


{-|

    Paginator.paginator
        { onChangeItem = Msg.ChangeItem
        , onPaginatorMsg = Msg.PaginatorMsg
        , totalAmount = 999
        , state = model.paginatorState
        }
        renderConfig

-}
paginator :
    { onChangeItem : Int -> msg
    , onChangePageAmount : Int -> msg
    , onToggleMenu : msg
    , totalAmount : Int
    , state : State
    }
    -> Paginator msg
paginator prop =
    Paginator prop
        { currentItem = 1
        , pageAmount = 25
        , amountOptions = [ 25, 50, 100 ]
        }



-- State


{-| Keep this one in your Model, it holds the paginator's state.
-}
type State
    = State StateModel


type alias StateModel =
    { menu : Bool }


{-| The correct way of instantiating a [`Paginator.State`](#State).

    { -- ...
    , paginatorState = Paginator.init
    -- ...
    }

-}
init : State
init =
    State
        { menu = False }


{-| Given a message, apply an update to the [`Paginator.State`](#State).

    newState =
        Paginator.update subMsg oldModel.paginatorState

-}
toggleMenu : State -> State
toggleMenu (State state) =
    State { state | menu = not state.menu }



-- Options


{-| The current item, most probably the first one displayed in your list.
Paginator.withCurrentItem 11 somePaginator
-}
withCurrentItem : Int -> Paginator msg -> Paginator msg
withCurrentItem item (Paginator prop opt) =
    Paginator prop { opt | currentItem = item }


{-| TODO: DOC
-}
withPageAmount : Int -> Paginator msg -> Paginator msg
withPageAmount pageAmount (Paginator prop opt) =
    Paginator prop { opt | pageAmount = pageAmount }


{-| TODO: DOC
-}
withPageAmountOptions : List Int -> Paginator msg -> Paginator msg
withPageAmountOptions amountOptions (Paginator prop opt) =
    Paginator prop { opt | amountOptions = amountOptions }



-- Rendering


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Paginator msg -> Element msg
renderElement renderConfig ((Paginator prop opt) as paginator_) =
    let
        { totalAmount } =
            prop

        { currentItem, pageAmount } =
            opt

        maxItemRange =
            currentItem + pageAmount
    in
    Keyed.row
        [ Element.width Element.fill ]
        [ rangeInfo renderConfig
            currentItem
            maxItemRange
            totalAmount
            |> Tuple.pair "current-range"
        , renderPaginator renderConfig paginator_
            |> Tuple.pair "indexer"
        , pageAmountSelector renderConfig paginator_
            |> Tuple.pair "page-amount-selector"
        ]


renderPaginator : RenderConfig -> Paginator msg -> Element msg
renderPaginator renderConfig ((Paginator prop opt) as paginator_) =
    let
        { onChangeItem, totalAmount } =
            prop

        { currentItem, pageAmount } =
            opt

        currentPage =
            floor <| toFloat currentItem / toFloat pageAmount

        isFistPage =
            currentPage <= 1

        isLastPage =
            nextPage >= pageAmount

        previousPage =
            (currentPage - 1) * pageAmount

        nextPage =
            (currentPage + 1) * pageAmount

        totalPages =
            ceiling <| toFloat totalAmount / toFloat pageAmount
    in
    Keyed.row
        [ Element.spacingXY 8 0
        , Element.centerX
        ]
        [ firstPageButton renderConfig
            (onChangeItem 1)
            isFistPage
            |> Tuple.pair "first-page"
        , previousButton renderConfig
            (onChangeItem previousPage)
            isFistPage
            |> Tuple.pair "previous-page"
        , renderPages renderConfig paginator_
            |> Tuple.pair "indexer"
        , nextButton
            renderConfig
            (onChangeItem nextPage)
            isLastPage
            |> Tuple.pair "next-page"
        , lastPageButton renderConfig
            (onChangeItem totalPages)
            isLastPage
            |> Tuple.pair "last-page"
        ]


renderPages : RenderConfig -> Paginator msg -> Element msg
renderPages renderConfig (Paginator prop opt) =
    let
        { onChangeItem, totalAmount } =
            prop

        { currentItem, pageAmount } =
            opt

        currentPage =
            currentItem // pageAmount

        maxItemRange =
            currentItem + pageAmount

        totalPages =
            ceiling <| toFloat totalAmount / toFloat pageAmount

        ( leftPageRange, currentPage_, rightPageRange ) =
            pageRanges totalPages maxItemRange currentPage
    in
    Keyed.row
        [ Element.spacingXY 2 0
        , Element.paddingXY 8 0
        ]
        [ leftPageRange
            |> List.map
                (changeIndexButton
                    renderConfig
                    currentItem
                    onChangeItem
                )
            |> Element.row []
            |> Tuple.pair "previous-range"
        , changeIndexButton
            renderConfig
            currentItem
            onChangeItem
            currentPage_
            |> Tuple.pair "current-page"
        , rightPageRange
            |> List.map
                (changeIndexButton
                    renderConfig
                    currentItem
                    onChangeItem
                )
            |> Element.row []
            |> Tuple.pair "next-range"
        ]


pageRanges : Int -> Int -> Int -> ( List Int, Int, List Int )
pageRanges total range page =
    let
        page_ =
            max page 1

        from =
            page_
                - range
                - max 0 ((page_ + range) - total)
                |> max 1

        to =
            page_
                + range
                + abs (min 0 (page_ - 1 - range))
                |> min total

        left =
            if page_ > 1 then
                List.range (max 1 from) (page_ - 1)

            else
                []

        right =
            if page_ < total then
                List.range (page_ + 1) (min total to)

            else
                []
    in
    ( left, page_, right )


rangeInfo : RenderConfig -> Int -> Int -> Int -> Element msg
rangeInfo renderConfig currentItem maxItemRange totalAmount =
    let
        paginatorFormat =
            renderConfig
                |> localeTerms
                |> .paginator
                |> .tableFormat
    in
    paginatorFormat
        { first = String.fromInt currentItem
        , last =
            String.fromInt
                (if maxItemRange >= totalAmount then
                    totalAmount

                 else
                    maxItemRange
                )
        , total = String.fromInt totalAmount
        }
        |> Text.body2
        |> Text.renderElement renderConfig
        |> Element.el [ Element.width <| Element.px 160 ]


previousButton : RenderConfig -> msg -> Bool -> Element msg
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


nextButton : RenderConfig -> msg -> Bool -> Element msg
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


firstPageButton : RenderConfig -> msg -> Bool -> Element msg
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


lastPageButton : RenderConfig -> msg -> Bool -> Element msg
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


changeIndexButton : RenderConfig -> Int -> (Int -> msg) -> Int -> Element msg
changeIndexButton renderConfig currentItem onChangeItem item =
    Button.fromLabel (String.fromInt item)
        |> Button.cmd (onChangeItem item)
            (if item == currentItem then
                Button.primary

             else
                Button.light
            )
        |> Button.withSize Size.small
        |> Button.renderElement renderConfig


pageAmountSelector : RenderConfig -> Paginator msg -> Element msg
pageAmountSelector renderConfig ((Paginator _ { amountOptions }) as paginator_) =
    if not <| List.isEmpty amountOptions then
        Keyed.row
            [ Element.width <| Element.px 160
            , Element.spacingXY 8 0
            ]
            [ Text.body2 "Rows/Page"
                |> Text.renderElement renderConfig
                |> Tuple.pair "rows-page"
            , paginator_
                |> amountMenu renderConfig
                |> Tuple.pair "amount-menu"
            ]

    else
        emptyAmountMenu


emptyAmountMenu : Element msg
emptyAmountMenu =
    Element.el
        [ Element.alignRight
        , Element.width <| Element.px 160
        ]
        Element.none


amountMenu : RenderConfig -> Paginator msg -> Element msg
amountMenu renderConfig (Paginator prop opt) =
    let
        { onToggleMenu, onChangePageAmount, state } =
            prop

        { amountOptions, pageAmount } =
            opt
    in
    Button.fromLabeledOnLeftIcon (Icon.chevronDown <| String.fromInt pageAmount)
        |> Button.cmd onToggleMenu Button.light
        |> Button.withSize Size.small
        |> Menu.menu onToggleMenu
            (List.map (amountMenuItem onChangePageAmount) amountOptions)
        |> Menu.setVisible (menuIsVisible state)
        |> Menu.renderElement renderConfig


amountMenuItem : (Int -> msg) -> Int -> MenuItem msg
amountMenuItem onChangePageAmount amountOption =
    amountOption
        |> String.fromInt
        |> Menu.item (onChangePageAmount amountOption) Nothing


menuIsVisible : State -> Bool
menuIsVisible (State { menu }) =
    menu
