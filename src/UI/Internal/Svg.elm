module UI.Internal.Svg exposing (animateSpin)

import Svg exposing (..)
import Svg.Attributes exposing (..)


animateSpin : Float -> Float -> Svg msg
animateSpin centerX centerY =
    let
        centerXStr =
            String.fromFloat centerX

        centerYStr =
            String.fromFloat centerY
    in
    animateTransform
        [ attributeName "transform"
        , type_ "rotate"
        , from <| "0 " ++ centerXStr ++ " " ++ centerYStr
        , to <| "360 " ++ centerXStr ++ " " ++ centerYStr
        , dur "1s"
        , repeatCount "indefinite"
        ]
        []
