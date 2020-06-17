module UI.Internal.NList exposing (..)

import UI.Internal.TypeNumbers as T


{-| The NList is a fixed-length list using phantom-type for compile-time constrains.
Besides fixed-lengths, it allows a parent type to constrain two or more lists to have the same number of elements.
One common use-case is using it for creating n-dimensional arrays, like matrixes/tables.
-}
type NList data n
    = NList (List data)


empty : NList msg T.Zero
empty =
    NList []


cons : data -> NList data n -> NList data (T.Increase n)
cons head_ (NList tail) =
    NList (head_ :: tail)


foldl : (data -> accu -> accu) -> accu -> NList data n -> accu
foldl applier accu (NList list) =
    List.foldl applier accu list


map : (a -> b) -> NList a n -> NList b n
map applier (NList list) =
    NList <| List.map applier list


map2 : (a -> b -> result) -> NList a n -> NList b n -> NList result n
map2 applier (NList xs) (NList ys) =
    NList <| List.map2 applier xs ys


toList : NList data n -> List data
toList (NList list) =
    list
