module UI.Theme exposing
    ( basePadding
    , baseSpacing
    , black
    , blackShade
    , borderColor
    , borderShadow
    , caption
    , error
    , gray1
    , gray2
    , gray3
    , gray4
    , headline
    , largePadding
    , largeSpacing
    , primary
    , regular
    , roundedBorder
    , sizings
    , smallPadding
    , smallSpacing
    , subheading
    , subtitle
    , success
    , tinyPadding
    , tinySpacing
    , title
    , warning
    , white
    , xLargePadding
    , xLargeSpacing
    )

import Element exposing (Attr, Attribute, Color)
import Element.Border as Border
import Element.Font as Font
import UI.Colors as Colors



-- Primary colors


primary : Color
primary =
    Colors.blue



-- Semantic colors


success : Color
success =
    Colors.green


error : Color
error =
    Colors.red


warning : Color
warning =
    Colors.yellow



-- Neutral colors


black : Color
black =
    Colors.black


blackShade : Color
blackShade =
    Colors.blackShade


white : Color
white =
    Colors.white


gray1 : Color
gray1 =
    Colors.gray1


gray2 : Color
gray2 =
    Colors.gray2


gray3 : Color
gray3 =
    Colors.gray3


gray4 : Color
gray4 =
    Colors.gray4



-- Sizes


headline : Attr decorative msg
headline =
    Font.size 20


subheading : Attr decorative msg
subheading =
    Font.size 14


title : Attr decorative msg
title =
    Font.size 16


subtitle : Attr decorative msg
subtitle =
    Font.size 10


caption : Attr decorative msg
caption =
    Font.size 8


regular : Attr decorative msg
regular =
    Font.size 12



-- Border


borderColor : Color
borderColor =
    gray4


borderShadow : Attr decorative msg
borderShadow =
    Border.shadow
        { offset = ( 0, 0 )
        , size = 0
        , blur = 10
        , color = gray4
        }


roundedBorder : Attribute msg
roundedBorder =
    Border.rounded 8



-- Spacing


sizings : { tiny : Int, small : Int, base : Int, large : Int, xLarge : Int }
sizings =
    { tiny = 8
    , small = 10
    , base = 20
    , large = 40
    , xLarge = 60
    }


tinySpacing : Attribute msg
tinySpacing =
    Element.spacing sizings.tiny


smallSpacing : Attribute msg
smallSpacing =
    Element.spacing sizings.small


baseSpacing : Attribute msg
baseSpacing =
    Element.spacing sizings.base


largeSpacing : Attribute msg
largeSpacing =
    Element.spacing sizings.large


xLargeSpacing : Attribute msg
xLargeSpacing =
    Element.spacing sizings.xLarge



-- Padding


tinyPadding : Attribute msg
tinyPadding =
    Element.padding sizings.tiny


smallPadding : Attribute msg
smallPadding =
    Element.padding sizings.small


basePadding : Attribute msg
basePadding =
    Element.padding sizings.base


largePadding : Attribute msg
largePadding =
    Element.padding sizings.large


xLargePadding : Attribute msg
xLargePadding =
    Element.padding sizings.xLarge
