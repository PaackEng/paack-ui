module UI.Internal.NArray exposing
    ( NArray(..)
    , empty
    , get
    , push
    , set
    , toList
    )

import Array exposing (Array)
import UI.Utils.TypeNumbers as T


{-| The NArray is a fixed-length array using phantom-type for compile-time constrains.
Besides fixed-lengths, it allows a parent type to constrain two or more lists to have the same number of elements.
One common use-case is using it for creating n-dimensional arrays, like matrixes/tables.
-}
type NArray data n
    = NArray (Array data)


empty : NArray msg T.Zero
empty =
    NArray Array.empty


get : Int -> NArray data n -> Maybe data
get index (NArray array) =
    Array.get index array


set : Int -> data -> NArray data n -> NArray data n
set index value (NArray old) =
    NArray (Array.set index value old)


push : data -> NArray data n -> NArray data (T.Increase n)
push new (NArray old) =
    NArray (Array.push new old)


toList : NArray data n -> List data
toList (NArray array) =
    Array.toList array
