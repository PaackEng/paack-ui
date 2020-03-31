module UI.Colors exposing
    ( black
    , blackShade
    , blue
    , gray1
    , gray2
    , gray3
    , gray4
    , green
    , red
    , white
    , yellow
    )

import Element exposing (Color, rgb, rgb255, rgba, rgba255)


blue : Color
blue =
    rgb255 18 71 208


yellow : Color
yellow =
    rgb255 255 241 12


green : Color
green =
    rgb255 40 255 112


red : Color
red =
    rgb255 251 9 55


black : Color
black =
    rgb255 7 11 20


blackShade : Color
blackShade =
    rgba255 0 0 0 0.1


white : Color
white =
    rgb255 255 255 255


gray1 : Color
gray1 =
    rgb255 57 57 57


gray2 : Color
gray2 =
    rgb255 150 150 150


gray3 : Color
gray3 =
    rgb255 222 222 222


gray4 : Color
gray4 =
    rgb255 244 244 244
