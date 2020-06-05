module UI.Size exposing (sizeExtraLarge, sizeLarge, sizeSmall)

import UI.Internal.ContextualSize as Internal exposing (ContextualSize)


sizeExtraLarge : ContextualSize
sizeExtraLarge =
    Internal.ExtraLarge


sizeLarge : ContextualSize
sizeLarge =
    Internal.Large


sizeSmall : ContextualSize
sizeSmall =
    Internal.Small
