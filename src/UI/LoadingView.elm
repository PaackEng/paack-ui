module UI.LoadingView exposing (big, default, small)

import Element exposing (Element)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


small : Element msg
small =
    generic 18


default : Element msg
default =
    generic 36


big : Element msg
big =
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
        , viewBox "0 0 38 38"
        , stroke "#1747CE"
        , id "loading-view"
        ]
        [ g
            [ fill "none"
            , fillRule "evenodd"
            ]
            [ g [ transform "translate(1 1)", strokeWidth "2" ]
                [ circle
                    [ strokeOpacity "0.5"
                    , cx "18"
                    , cy "18"
                    , r "18"
                    ]
                    []
                , Svg.path [ d "M36 18c0-9.94-8.06-18-18-18" ]
                    [ animateTransform
                        [ attributeName "transform"
                        , type_ "rotate"
                        , from "0 18 18"
                        , to "360 18 18"
                        , dur "1s"
                        , repeatCount "indefinite"
                        ]
                        []
                    ]
                ]
            ]
        ]
