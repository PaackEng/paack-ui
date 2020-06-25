module UI.Paginator exposing
    ( PaginatorConfig
    , nonNumeric
    )

{-| `UI.Paginator` is a component for helping navigation in a large sample of elements.

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
            { onNextButtonClicked = Msg.NextPage
            , onPreviousButtonClicked = Msg.PreviousPage
            , totalCount = List.length model.options
            , offset = model.pageOffset
            , first = 0
            }
            renderConfig
        ]


# Building

@docs PaginatorConfig


# Rendering

@docs nonNumeric

-}

import Element exposing (Element, fill)
import UI.Button as Button exposing (Button)
import UI.Icon as Icon exposing (Icon)
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.Element exposing (zeroPadding)


{-| `PaginatorConfig` holds the configuration required to present a Paginator.

    { onNextButtonClicked = Msg.NextPage
    , onPreviousButtonClicked = Msg.PreviousPage
    , totalCount = List.length model.options
    , offset = model.pageOffset
    , first = 0
    }

-}
type alias PaginatorConfig msg =
    { onNextButtonClicked : msg
    , onPreviousButtonClicked : msg
    , totalCount : Int
    , offset : Int
    , first : Int
    }


{-| Given a [`PaginatorConfig`](UI-Paginator#PaginatorConfig), renders paginator.
This paginator style (non-numeric) has only the next and previous buttons. In between those, it presents the current offset.

    Paginator.nonNumeric
        paginatorConfig
        renderConfig

-}
nonNumeric : PaginatorConfig msg -> RenderConfig -> Element msg
nonNumeric ({ first, offset, totalCount } as paginator) renderConfig =
    let
        firstItemCount =
            offset
                + 1
                |> String.fromInt

        lastItemCount =
            String.fromInt <|
                if (offset + first) > totalCount then
                    totalCount

                else
                    offset + first

        noPrevious =
            offset == 0

        noNext =
            first + offset >= totalCount
    in
    Element.row
        []
        [ Text.body2
            (firstItemCount
                ++ "-"
                ++ lastItemCount
                ++ " of "
                ++ String.fromInt totalCount
            )
            |> Text.renderElement renderConfig
            |> Element.el
                [ Element.paddingEach
                    { zeroPadding | right = 80 }
                , Element.width fill
                ]
        , button paginator.onPreviousButtonClicked
            noPrevious
            (Icon.previousContent "Previous")
            |> Button.renderElement renderConfig
            |> Element.el
                [ Element.paddingEach
                    { zeroPadding | right = 8 }
                ]
        , button paginator.onNextButtonClicked
            noNext
            (Icon.nextContent "Next")
            |> Button.renderElement renderConfig
        ]


button : msg -> Bool -> Icon -> Button msg
button msg isDisabled icon =
    Button.fromIcon icon
        |> Button.cmd msg Button.light
        |> Button.withDisabledIf isDisabled
