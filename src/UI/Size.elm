module UI.Size exposing
    ( extraSmall, small, medium, large
    , Size
    )

{-| Contextual size for buttons and icons, as described in the design system.


# Sizes

@docs extraSmall, small, medium, large

-}

import UI.Internal.Size as Internal


{-| Upholds a size value.
-}
type alias Size =
    Internal.Size


{-| For extra-small-sized components.
-}
extraSmall : Size
extraSmall =
    Internal.ExtraSmall


{-| For large-sized components.
-}
large : Size
large =
    Internal.Large


{-| For small-sized components.
-}
small : Size
small =
    Internal.Small


{-| For medium-sized components.

**NOTE**: This is the default value.

-}
medium : Size
medium =
    Internal.Medium
