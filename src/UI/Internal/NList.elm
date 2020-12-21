module UI.Internal.NList exposing (NList(..))

{-| The NList is a fixed-length list using phantom-type for compile-time constrains.
Besides fixed-lengths, it allows a parent type to constrain two or more lists to have the same number of elements.
One common use-case is using it for creating n-dimensional arrays, like matrixes/tables.
-}


type NList data n
    = NList (List data)
