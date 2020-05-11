module UI.Internal.Palette exposing (..)

import Element exposing (rgb255)



-- Tones


type alias ToneColors =
    { darkest : Element.Color
    , middle : Element.Color
    , light : Element.Color
    , lighter : Element.Color
    , lightest : Element.Color
    }


gray : ToneColors
gray =
    { darkest = rgb255 14 20 32 -- #0E1420
    , middle = rgb255 74 74 74 -- #4A4A4A
    , light = rgb255 150 150 150 -- #969696
    , lighter = rgb255 228 228 228 -- #E4E4E4
    , lightest = rgb255 246 246 246 -- #F6F6F6
    }


primary : ToneColors
primary =
    { darkest = rgb255 12 66 156 -- #0C429C
    , middle = rgb255 27 96 216 -- #1B60D8
    , light = rgb255 81 145 255 -- #5191FF
    , lighter = rgb255 147 187 255 -- #93BBFF
    , lightest = rgb255 227 238 255 -- #E3EEFF
    }


success : ToneColors
success =
    { darkest = rgb255 26 158 86 -- #1A9E56
    , middle = rgb255 34 229 123 -- #22E57B
    , light = rgb255 95 255 168 -- #5FFFA8
    , lighter = rgb255 183 255 216 -- #B7FFD8
    , lightest = rgb255 220 255 236 -- #DCFFEC
    }


danger : ToneColors
danger =
    { darkest = rgb255 182 0 24 -- #B60018
    , middle = rgb255 255 44 72 -- #FF2C48
    , light = rgb255 255 94 116 -- #FF5E74
    , lighter = rgb255 255 174 185 -- #FFAEB9
    , lightest = rgb255 255 218 223 -- #FFDADF
    }


warning : ToneColors
warning =
    { darkest = rgb255 148 133 0 -- #948500
    , middle = rgb255 252 226 1 -- #FCE201
    , light = rgb255 255 236 68 -- #FFEC44
    , lighter = rgb255 255 245 157 -- #FFF59D
    , lightest = rgb255 255 249 201 -- #FFF9C9
    }



-- Text


textBgMiddle : Element.Color
textBgMiddle =
    -- #FFF
    rgb255 255 255 255


textBgLightest : Element.Color
textBgLightest =
    -- #1B60D8
    rgb255 27 96 216


textBgLighter : Element.Color
textBgLighter =
    -- #0C429C
    rgb255 12 66 156


textComponentDisabled : Element.Color
textComponentDisabled =
    -- #A5A5A5
    rgb255 165 165 165
