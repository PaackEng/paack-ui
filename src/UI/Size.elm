module UI.Size exposing (sizeExtraLarge, sizeLarge, sizeSmall)

import UI.Internal.Size as Internal exposing (ContextualSize)


sizeExtraLarge : ContextualSize
sizeExtraLarge =
    Internal.SizeExtraLarge


sizeLarge : ContextualSize
sizeLarge =
    Internal.SizeLarge


sizeSmall : ContextualSize
sizeSmall =
    Internal.SizeSmall
