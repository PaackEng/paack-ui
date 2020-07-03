module UI.Internal.NArray exposing (..)

import Array exposing (Array)
import UI.Internal.NList exposing (NList(..))
import UI.Internal.TypeNumbers as T


{-| The NArray is a fixed-length array using phantom-type for compile-time constrains.
Besides fixed-lengths, it allows a parent type to constrain two or more lists to have the same number of elements.
One common use-case is using it for creating n-dimensional arrays, like matrixes/tables.
-}
type NArray data n
    = NArray (Array data)


empty : NArray msg T.Zero
empty =
    NArray Array.empty


length : NArray msg columns -> Int
length (NArray array) =
    Array.length array


get : Int -> NArray data n -> Maybe data
get index (NArray array) =
    Array.get index array


set : Int -> data -> NArray data n -> NArray data n
set index value (NArray old) =
    NArray (Array.set index value old)


push : data -> NArray data n -> NArray data (T.Increase n)
push new (NArray old) =
    NArray (Array.push new old)


foldl : (data -> accu -> accu) -> accu -> NArray data n -> accu
foldl applier accu (NArray array) =
    Array.foldl applier accu array


map : (a -> b) -> NArray a n -> NArray b n
map applier (NArray array) =
    NArray <| Array.map applier array


toNList : NArray data n -> NList data n
toNList (NArray array) =
    NList <| Array.toList array


toIndexedNList : NArray data n -> NList ( Int, data ) n
toIndexedNList (NArray array) =
    NList <| Array.toIndexedList array


toList : NArray data n -> List data
toList (NArray array) =
    Array.toList array


toArray : NArray data n -> Array data
toArray (NArray array) =
    array
