module UI.Paginator exposing (PaginatorConfig, nonNumeric)

import Element exposing (Element, fill)
import UI.Button as Button exposing (Button, ButtonMode)
import UI.Icon as Icon exposing (Icon)
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.Element exposing (zeroPadding)



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

        previousStateMode =
            if offset > 0 then
                Button.modeEnabled

            else
                Button.modeDisabled

        nextStateMode =
            if first + offset < totalCount then
                Button.modeEnabled

            else
                Button.modeDisabled
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
            |> Text.toEl renderConfig
            |> Element.el
                [ Element.paddingEach
                    { zeroPadding | right = 80 }
                , Element.width fill
                ]
        , button paginator.onPreviousButtonClicked
            previousStateMode
            (Icon.leftArrow "Previous")
            |> Button.toEl renderConfig
            |> Element.el
                [ Element.paddingEach
                    { zeroPadding | right = 8 }
                ]
        , button paginator.onNextButtonClicked
            nextStateMode
            (Icon.rightArrow "Next")
            |> Button.toEl renderConfig
        ]


button : msg -> ButtonMode -> Icon -> Button msg
button msg buttonMode icon =
    Button.bodyIcon icon
        |> Button.button msg
        |> Button.withTone Button.toneLight
        |> Button.withMode buttonMode
        |> Button.withTone Button.toneLight
