module UI.Internal.Colors exposing
    ( ToneColors
    , contrastDanger
    , contrastPrimary
    , contrastSuccess
    , contrastWarning
    , danger
    , darkConstrast
    , gray
    , mainBackground
    , overlayBackground
    , primary
    , success
    , warning
    , white
    )

import Element exposing (Attribute, rgb255)
import Element.Background as Background



-- Tones


type alias ToneColors =
    { dark1 : Element.Color
    , middle : Element.Color
    , light1 : Element.Color
    , light2 : Element.Color
    , light3 : Element.Color
    , light4 : Element.Color
    }


gray : ToneColors
gray =
    { dark1 = rgb255 14 20 32 -- #0E1420
    , middle = rgb255 74 74 74 -- #4A4A4A
    , light1 = rgb255 136 136 136 -- #888888
    , light2 = rgb255 228 228 228 -- #E4E4E4
    , light3 = rgb255 246 246 246 -- #F6F6F6
    , light4 = rgb255 250 250 250 -- #FAFAFA
    }


primary : ToneColors
primary =
    { dark1 = rgb255 12 66 156 -- #0C429C
    , middle = rgb255 27 96 216 -- #1B60D8
    , light1 = rgb255 81 145 255 -- #5191FF
    , light2 = rgb255 147 187 255 -- #93BBFF
    , light3 = rgb255 227 238 255 -- #E3EEFF
    , light4 = rgb255 244 248 255 -- #F4F8FF
    }


success : ToneColors
success =
    { dark1 = rgb255 1 167 76 -- #01A74C
    , middle = rgb255 34 229 123 -- #22E57B
    , light1 = rgb255 95 255 168 -- #5FFFA8
    , light2 = rgb255 183 255 216 -- #B7FFD8
    , light3 = rgb255 220 255 236 -- #DCFFEC
    , light4 = rgb255 242 255 248 -- #F2FFF8
    }


danger : ToneColors
danger =
    { dark1 = rgb255 182 0 24 -- #B60018
    , middle = rgb255 239 19 49 -- #EF1331
    , light1 = rgb255 255 94 116 -- #FF5E74
    , light2 = rgb255 255 174 185 -- #FFAEB9
    , light3 = rgb255 255 232 235 -- #FFE8EB
    , light4 = rgb255 255 245 246 -- #FFF5F6
    }


warning : ToneColors
warning =
    { dark1 = rgb255 148 133 0 -- #948500
    , middle = rgb255 252 226 1 -- #FCE201
    , light1 = rgb255 255 236 68 -- #FFEC44
    , light2 = rgb255 255 245 157 -- #FFF59D
    , light3 = rgb255 255 249 201 -- #FFF9C9
    , light4 = rgb255 255 252 232 -- #FFFCE8
    }


white : Element.Color
white =
    rgb255 255 255 255



-- Contrasted Tones


darkConstrast : ToneColors
darkConstrast =
    { dark1 = white
    , middle = white
    , light1 = white
    , light2 = gray.dark1
    , light3 = gray.dark1
    , light4 = gray.dark1
    }


lightContrast : ToneColors
lightContrast =
    { dark1 = white
    , middle = gray.dark1
    , light1 = gray.dark1
    , light2 = gray.dark1
    , light3 = gray.dark1
    , light4 = gray.dark1
    }


contrastPrimary : ToneColors
contrastPrimary =
    darkConstrast


contrastSuccess : ToneColors
contrastSuccess =
    lightContrast


contrastDanger : ToneColors
contrastDanger =
    darkConstrast


contrastWarning : ToneColors
contrastWarning =
    lightContrast



-- Text
-- Background


mainBackground : Attribute msg
mainBackground =
    Background.color white


overlayBackground : Attribute msg
overlayBackground =
    Background.color (Element.rgba255 0 0 0 0.5)
