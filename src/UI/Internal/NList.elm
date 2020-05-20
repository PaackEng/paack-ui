module UI.Internal.NList exposing (..)

import UI.Internal.TypeNumbers as T


type NList data n
    = NList (List data)


empty : NList msg T.Zero
empty =
    NList []


cons : data -> NList data n -> NList data (T.Inc n)
cons head_ (NList tail) =
    NList (head_ :: tail)


foldl : (data -> acu -> acu) -> acu -> NList data n -> acu
foldl applier acu (NList list) =
    List.foldl applier acu list


map : (a -> b) -> NList a n -> NList b n
map applier (NList list) =
    NList <| List.map applier list


map2 : (a -> b -> result) -> NList a n -> NList b n -> NList result n
map2 applier (NList xs) (NList ys) =
    NList <| List.map2 applier xs ys


toList : NList data n -> List data
toList (NList list) =
    list
