module UI.Internal.Palette exposing
    ( ToneColors
    , danger
    , gray
    , primary
    , success
    , warning
    )

import Element exposing (Color, rgb255)


type alias ToneColors =
    { darkest : Color
    , middle : Color
    , light : Color
    , lighter : Color
    , lightest : Color
    }


gray : ToneColors
gray =
    { darkest = rgb255 14 20 32
    , middle = rgb255 74 74 74
    , light = rgb255 165 165 165
    , lighter = rgb255 228 228 228
    , lightest = rgb255 246 246 246
    }


primary : ToneColors
primary =
    { darkest = rgb255 12 66 156
    , middle = rgb255 27 96 216
    , light = rgb255 81 145 255
    , lighter = rgb255 147 187 255
    , lightest = rgb255 227 238 255
    }


success : ToneColors
success =
    { darkest = rgb255 26 158 86
    , middle = rgb255 34 229 123
    , light = rgb255 95 255 168
    , lighter = rgb255 95 255 168
    , lightest = rgb255 220 255 236
    }


danger : ToneColors
danger =
    { darkest = rgb255 182 0 24
    , middle = rgb255 255 44 72
    , light = rgb255 255 44 72
    , lighter = rgb255 255 44 72
    , lightest = rgb255 255 44 72
    }


warning : ToneColors
warning =
    { darkest = rgb255 148 133 0
    , middle = rgb255 252 226 1
    , light = rgb255 252 226 1
    , lighter = rgb255 255 245 157
    , lightest = rgb255 255 249 201
    }
