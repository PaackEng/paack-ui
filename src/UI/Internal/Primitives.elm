module UI.Internal.Primitives exposing (..)

import Element exposing (Attribute)
import Element.Border as Border
import UI.Internal.Size as Size exposing (Size)


defaultRoundedBorders : Attribute msg
defaultRoundedBorders =
    Border.rounded 8


roundedBorders : Size -> Attribute msg
roundedBorders size =
    Border.rounded <|
        case size of
            Size.Large ->
                8

            Size.Medium ->
                6

            Size.Small ->
                5

            Size.ExtraSmall ->
                4


textFieldPadding : Size -> Attribute msg
textFieldPadding size =
    case size of
        Size.Large ->
            -- TODO: NOT Specified in Core Design System
            Element.paddingXY 24 22

        Size.Medium ->
            Element.paddingXY 18 16

        Size.Small ->
            -- TODO: NOT Specified in Core Design System
            Element.paddingXY 12 10

        Size.ExtraSmall ->
            Element.paddingXY 8 5
