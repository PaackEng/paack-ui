module UI.Internal.Palette exposing
    ( Color
    , ToneColors
    , danger
    , gray
    , primary
    , success
    , textDisbledWithGrayLightest
    , textWithBg
    , textWithGrayLighter
    , textWithGrayLightest
    , warning
    )

import Element exposing (rgb255)


type alias Color =
    Element.Color



-- Tones


type alias ToneColors =
    { darkest : Color
    , middle : Color
    , light : Color
    , lighter : Color
    , lightest : Color
    }


gray : ToneColors
gray =
    { darkest = rgb255 14 20 32 -- #0E1420
    , middle = rgb255 74 74 74 -- #4A4A4A
    , light = rgb255 165 165 165 -- #A5A5A5
    , lighter = rgb255 228 228 228 -- #E4E4E4
    , lightest = rgb255 246 246 246 -- #F6F6F6
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



-- Text


textWithBg : Color
textWithBg =
    -- #FFF
    rgb255 255 255 255


textWithGrayLightest : Color
textWithGrayLightest =
    -- #1B60D8
    rgb255 27 96 216


textWithGrayLighter : Color
textWithGrayLighter =
    -- #0C429C
    rgb255 12 66 156


textDisbledWithGrayLightest : Color
textDisbledWithGrayLightest =
    -- #A5A5A5
    rgb255 165 165 165
