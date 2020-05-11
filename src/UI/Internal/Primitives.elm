module UI.Internal.Primitives exposing (roundedBorders, roundedFields)

import Element exposing (Attribute)
import Element.Border as Border


roundedBorders : Attribute msg
roundedBorders =
    Border.rounded 8


roundedFields : Attribute msg
roundedFields =
    Border.rounded 4
