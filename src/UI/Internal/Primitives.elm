module UI.Internal.Primitives exposing (roundedBorders)

import Element exposing (Attribute)
import Element.Border as Border


roundedBorders : Attribute msg
roundedBorders =
    Border.rounded 8
