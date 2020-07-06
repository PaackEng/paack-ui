module UI.Internal.Primitives exposing (defaultRoundedBorders, roundedBorders)

import Element exposing (Attribute)
import Element.Border as Border
import UI.Internal.Size as Size exposing (Size)


defaultRoundedBorders : Attribute msg
defaultRoundedBorders =
    Border.rounded 8


{-| REVIEW: I don't think I liked this name, gimme suggestions for this!
-}
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
