module UI.Paginator exposing (PaginatorConfig, nonNumeric)

import Element exposing (Element, fill)
import UI.Button as Button exposing (Button)
import UI.Icon as Icon exposing (Icon)
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.Element exposing (zeroPadding)
import Utils exposing (iconsSvgSprite)



{-
   first: N Return only the first N results
   offset: N Skip the first N results
-}


type alias PaginatorConfig msg =
    { onNextButtonClicked : msg
    , onPreviousButtonClicked : msg
    , totalCount : Int
    , offset : Int
    , first : Int
    }


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
        [ iconsSvgSprite
        , Text.body2
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
            (Icon.leftArrow "Previous")
            |> Button.renderElement renderConfig
            |> Element.el
                [ Element.paddingEach
                    { zeroPadding | right = 8 }
                ]
        , button paginator.onNextButtonClicked
            noNext
            (Icon.rightArrow "Next")
            |> Button.renderElement renderConfig
        ]


button : msg -> Bool -> Icon -> Button msg
button msg isDisabled icon =
    Button.fromIcon icon
        |> Button.cmd msg Button.light
        |> Button.withDisabledIf isDisabled
