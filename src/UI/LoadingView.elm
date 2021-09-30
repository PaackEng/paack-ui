module UI.LoadingView exposing (medium, small, large)

{-| The `UI.LoadingView` is a view with a loading spinner and nothing else.
It is pretty useful when a view depends on external content fetching.


# Different sizes

@docs medium, small, large

-}

import Element exposing (Element)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import UI.Internal.Svg as Svg
import UI.Palette as Palette


{-| The version with a small (18x18px) spinner.
-}
small : Element msg
small =
    generic 18


{-| The version with a medium (36x36px) spinner.
-}
medium : Element msg
medium =
    generic 36


{-| The version with a large (72x72px) spinner.
-}
large : Element msg
large =
    generic 72


generic : Float -> Element msg
generic size =
    spinnerSvg size
        |> Element.html
        |> Element.el [ Element.centerX, Element.centerY ]



-- Reference: https://github.com/SamHerbert/SVG-Loaders/blob/master/svg-loaders/oval.svg


spinnerSvg : Float -> Html msg
spinnerSvg floatSize =
    let
        size =
            String.fromFloat floatSize
    in
    svg
        [ width size
        , height size
        , viewBox "-1 -1 40 40"
        , stroke (Palette.blue800 |> Palette.toCssColor)
        , id "loading-view"
        ]
        [ g
            [ fill "none"
            , fillRule "evenodd"
            ]
            [ g [ transform "translate(1 1)", strokeWidth "3" ]
                [ circle
                    [ strokeOpacity "0.1"
                    , cx "18"
                    , cy "18"
                    , r "18"
                    ]
                    []
                , Svg.path [ d "M36 18c0-9.94-8.06-18-18-18" ]
                    [ Svg.animateSpin 18 18 ]
                ]
            ]
        ]
