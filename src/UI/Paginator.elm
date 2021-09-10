module UI.Paginator exposing
    ( Paginator, nonNumeric
    , withCurrentItem, withCurrentPage
    , renderElement
    )

{-| `UI.Paginator` is a component for helping navigation in a large sample of elements.
It provides navigation buttons and current location information.

A paginator does not include the logic required for taking/dropping the source of elements, and neither does the rendering of these elements.
The following code applies the paginator to some simple list, and also applies paginating logic on it:

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
            , current = 0
            }
            renderConfig
        ]


# Building

@docs Paginator, nonNumeric


# Options

@docs withCurrentItem, withCurrentPage


# Rendering

@docs renderElement

-}

import Element exposing (Element, fill)
import UI.Button as Button exposing (Button)
import UI.Icon as Icon exposing (Icon)
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text
import UI.Utils.Element exposing (zeroPadding)


type alias Properties msg =
    { onForwardClicked : msg
    , onPreviousClicked : msg
    , totalAmount : Int
    , pageAmount : Int
    }


type alias Options =
    { current : Int }


type Paginator
    = NonNumeric Properties Options


{-| This paginator style has a label, followed by the previous and next buttons.

The label looks like: {{current + 1}} - {{min (pageAmount + current) max)}} of {{max}}

    Paginator.nonNumeric
        paginatorConfig
        renderConfig

-}
nonNumeric : Paginator msg
nonNumeric =
    NonNumeric


withCurrentItem : Int -> Paginator msg -> Paginator msg
withCurrentItem value (NonNumeric prop opt) =
    nonNumeric prop { opt | current = value }


withCurrentPage : Int -> Paginator msg -> Paginator msg
withCurrentPage pageNumber (NonNumeric prop opt) =
    nonNumeric prop { opt | current = pageNumber * pageAmount }


renderElement : RenderConfig -> Paginator msg -> Element msg
renderElement renderConfig (NonNumeric prop { current }) =
    let
        currentItemCount =
            pageAmount
                + 1
                |> String.fromInt

        lastItemCount =
            String.fromInt <|
                if (pageAmount + current) > totalAmount then
                    totalAmount

                else
                    pageAmount + current

        noPrevious =
            pageAmount == 0

        noNext =
            current + pageAmount >= totalAmount

        paginatorsTerms =
            renderConfig |> localeTerms >> .paginator
    in
    Element.row
        []
        [ paginatorsTerms.format
            { current = currentItemCount
            , last = lastItemCount
            , total = String.fromInt totalAmount
            }
            |> Text.body2
            |> Text.renderElement renderConfig
            |> Element.el
                [ Element.paddingEach
                    { zeroPadding | right = 80 }
                , Element.width fill
                ]
        , button paginator.onPreviousClicked
            noPrevious
            (Icon.previousContent paginatorsTerms.previous)
            |> Button.renderElement renderConfig
            |> Element.el
                [ Element.paddingEach
                    { zeroPadding | right = 8 }
                ]
        , button paginator.onForwardClicked
            noNext
            (Icon.nextContent paginatorsTerms.next)
            |> Button.renderElement renderConfig
        ]


button : msg -> Bool -> Icon -> Button msg
button msg isDisabled icon =
    Button.fromIcon icon
        |> Button.cmd msg Button.light
        |> Button.withDisabledIf isDisabled
        |> Button.withSize Size.small
