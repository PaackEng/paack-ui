module UI.Internal.Size exposing (Size(..), default)


type Size
    = Large
    | Medium
    | Small
    | ExtraSmall


default : Size
default =
    Medium
