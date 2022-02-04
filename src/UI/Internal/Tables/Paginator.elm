module UI.Internal.Tables.Paginator exposing
    ( Paginator, paginator
    , State, Msg, init, update
    , withCurrentItem, withPageAmountOptions
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

@docs State, Msg, init, update


# Options

@docs withCurrentItem, withPageAmountOptions


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
    , onPaginatorMsg : Msg -> msg
    , totalAmount : Int
    , state : State
    }


type alias Options =
    { currentItem : Int
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
    , onPaginatorMsg : Msg -> msg
    , totalAmount : Int
    , state : State
    }
    -> Paginator msg
paginator prop =
    Paginator prop
        { currentItem = 1
        , amountOptions = [ 25, 50, 100 ]
        }



-- State


type Msg
    = ChangeAmount Int
    | ToggleMenu


{-| Keep this one in your Model, it holds the paginator's state.
-}
type State
    = State StateModel


type alias StateModel =
    { pageAmount : Int
    , menu : Bool
    }


{-| The correct way of instantiating a [`Paginator.State`](#State).

    { -- ...
    , paginatorState = Paginator.init
    -- ...
    }

-}
init : State
init =
    State
        { pageAmount = 25
        , menu = False
        }


{-| Given a message, apply an update to the [`Paginator.State`](#State).

    newState =
        Paginator.update subMsg oldModel.paginatorState

-}
update : Msg -> State -> State
update msg (State state) =
    case msg of
        ChangeAmount amount ->
            State { state | pageAmount = amount }

        ToggleMenu ->
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
withPageAmountOptions : List Int -> Paginator msg -> Paginator msg
withPageAmountOptions amountOptions (Paginator prop opt) =
    Paginator prop { opt | amountOptions = amountOptions }



-- Rendering


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Paginator msg -> Element msg
renderElement renderConfig (Paginator { onChangeItem, onPaginatorMsg, totalAmount, state } { currentItem, amountOptions }) =
    let
        (State { pageAmount }) =
            state

        currentPage =
            currentItem // pageAmount

        previousPage =
            (currentPage - 1) * pageAmount

        nextPage =
            (currentPage + 1) * pageAmount

        maxItemRange =
            currentItem + pageAmount

        totalPages =
            ceiling <| toFloat totalAmount / toFloat pageAmount

        ( leftPageRange, rightPageRange ) =
            pageRanges totalPages maxItemRange currentPage
    in
    Keyed.row []
        [ ( "current-range"
          , rangeInfo renderConfig
                currentItem
                maxItemRange
                totalAmount
          )
        , ( "first-page"
          , firstPageButton renderConfig
                (onChangeItem 1)
          )
        , ( "previous-page"
          , previousButton renderConfig
                (onChangeItem previousPage)
                (previousPage < 1)
          )
        , ( "previous-range"
          , leftPageRange
                |> List.map
                    (changeIndexButton
                        renderConfig
                        currentItem
                        onChangeItem
                    )
                |> Element.row []
          )
        , ( "current-page"
          , changeIndexButton
                renderConfig
                currentItem
                onChangeItem
                currentPage
          )
        , ( "next-range"
          , rightPageRange
                |> List.map
                    (changeIndexButton
                        renderConfig
                        currentItem
                        onChangeItem
                    )
                |> Element.row []
          )
        , ( "next-page"
          , nextButton
                renderConfig
                (onChangeItem nextPage)
                (nextPage > pageAmount)
          )
        , ( "last-page"
          , lastPageButton renderConfig
                (onChangeItem totalPages)
          )
        , ( "page-amount-selector"
          , case amountOptions of
                [] ->
                    Element.none

                _ ->
                    pageAmountSelector renderConfig onPaginatorMsg amountOptions pageAmount
          )
        ]


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


firstPageButton : RenderConfig -> msg -> Element msg
firstPageButton renderConfig msg =
    renderConfig
        |> localeTerms
        |> .paginator
        |> .first
        |> Button.fromLabel
        -- TODO
        |> Button.cmd msg
            Button.light
        |> Button.withSize Size.small
        |> Button.renderElement renderConfig


lastPageButton : RenderConfig -> msg -> Element msg
lastPageButton renderConfig msg =
    renderConfig
        |> localeTerms
        |> .paginator
        |> .last
        |> Button.fromLabel
        |> Button.cmd msg
            Button.light
        |> Button.withSize Size.small
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


pageRanges : Int -> Int -> Int -> ( List Int, List Int )
pageRanges total range page =
    let
        from =
            page
                - range
                - max 0 ((page + range) - total)
                |> max 1

        to =
            page
                + range
                + abs (min 0 (page - 1 - range))
                |> min total

        left =
            if page > 1 then
                List.range (max 1 from) (page - 1)

            else
                []

        right =
            if page < total then
                List.range (page + 1) (min total to)

            else
                []
    in
    ( left, right )


pageAmountSelector : RenderConfig -> (Msg -> msg) -> List Int -> Int -> Element msg
pageAmountSelector renderConfig onPaginatorMsg amountOptions pageAmount =
    Keyed.row []
        [ ( "rows-page"
          , Text.body2 "Rows/Page"
                |> Text.renderElement renderConfig
          )
        , ( "amount-menu"
          , amountMenu renderConfig onPaginatorMsg amountOptions pageAmount
          )
        ]


amountMenu : RenderConfig -> (Msg -> msg) -> List Int -> Int -> Element msg
amountMenu renderConfig onPaginatorMsg amountOptions pageAmount =
    Button.fromLabel (String.fromInt pageAmount)
        |> Button.cmd (onPaginatorMsg ToggleMenu) Button.light
        |> Button.withSize Size.small
        |> Menu.menu (onPaginatorMsg ToggleMenu)
            (List.map (amountMenuItem (ChangeAmount >> onPaginatorMsg))
                amountOptions
            )
        |> Menu.renderElement renderConfig


amountMenuItem : (Int -> msg) -> Int -> MenuItem msg
amountMenuItem onChangePageAmount amountOption =
    amountOption
        |> String.fromInt
        |> Menu.item (onChangePageAmount amountOption) Nothing
