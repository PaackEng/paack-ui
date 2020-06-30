module UI.Internal.Primitives exposing (relativeRoundedBorders, roundedBorders)

import Element exposing (Attribute)
import Element.Border as Border
import UI.Internal.Size as Size exposing (Size)


roundedBorders : Attribute msg
roundedBorders =
    Border.rounded 8


{-| REVIEW: I don't think I liked this name, gimme suggestions for this!
-}
relativeRoundedBorders : Size -> Attribute msg
relativeRoundedBorders size =
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
