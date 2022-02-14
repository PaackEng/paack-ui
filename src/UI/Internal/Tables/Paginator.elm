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
        , pageAmount = 15
        , amountOptions = [ 15, 25, 50, 100 ]
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
renderElement renderConfig paginator_ =
    Keyed.row
        [ Element.width Element.fill ]
        [ rangeInfo renderConfig paginator_
            |> Tuple.pair "current-range"
        , renderPaginator renderConfig paginator_
            |> Tuple.pair "indexer"
        , pageAmountSelector renderConfig paginator_
            |> Tuple.pair "page-amount-selector"
        ]


rangeInfo : RenderConfig -> Paginator msg -> Element msg
rangeInfo renderConfig (Paginator { totalAmount } { currentItem, pageAmount }) =
    let
        paginatorFormat =
            renderConfig
                |> localeTerms
                |> .paginator
                |> .tableFormat
    in
    paginatorFormat
        { first = String.fromInt currentItem
        , last = String.fromInt (min (currentItem + pageAmount) totalAmount)
        , total = String.fromInt totalAmount
        }
        |> Text.body2
        |> Text.renderElement renderConfig
        |> Element.el [ Element.width <| Element.px 160 ]


renderPaginator : RenderConfig -> Paginator msg -> Element msg
renderPaginator renderConfig ((Paginator prop opt) as paginator_) =
    let
        { onChangeItem, totalAmount } =
            prop

        { currentItem, pageAmount } =
            opt

        currentPage =
            ceiling <| toFloat currentItem / toFloat pageAmount

        isFistPage =
            currentPage <= 1

        isLastPage =
            currentPage >= ceiling (toFloat totalAmount / toFloat pageAmount)
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
            (onChangeItem <| (currentPage - 1) * pageAmount)
            isFistPage
            |> Tuple.pair "previous-page"
        , renderPages renderConfig paginator_
            |> Tuple.pair "indexer"
        , nextButton
            renderConfig
            (onChangeItem <| (currentPage + 1) * pageAmount)
            isLastPage
            |> Tuple.pair "next-page"
        , lastPageButton renderConfig
            (onChangeItem <| ceiling <| toFloat totalAmount / toFloat pageAmount)
            isLastPage
            |> Tuple.pair "last-page"
        ]


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


renderPages : RenderConfig -> Paginator msg -> Element msg
renderPages renderConfig ((Paginator { totalAmount } { currentItem, pageAmount }) as paginator_) =
    let
        currentPage =
            currentItem // pageAmount

        ( leftPageRange, rightPageRange ) =
            pageRanges
                (ceiling <| toFloat totalAmount / toFloat pageAmount)
                pageAmount
                currentPage
    in
    Keyed.row
        [ Element.spacingXY 2 0
        , Element.paddingXY 8 0
        ]
        [ leftPageRange
            |> List.map (changeIndexButton renderConfig paginator_ False)
            |> Element.row []
            |> Tuple.pair "previous-range"
        , changeIndexButton renderConfig paginator_ True currentPage
            |> Tuple.pair "current-page"
        , rightPageRange
            |> List.map (changeIndexButton renderConfig paginator_ False)
            |> Element.row []
            |> Tuple.pair "next-range"
        ]


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


changeIndexButton : RenderConfig -> Paginator msg -> Bool -> Int -> Element msg
changeIndexButton renderConfig (Paginator { onChangeItem } { pageAmount }) currentPage item =
    Button.fromLabel (String.fromInt <| item + 1)
        |> Button.cmd (onChangeItem <| item * pageAmount)
            (if currentPage then
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
