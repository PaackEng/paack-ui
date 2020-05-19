module UI.Internal.Basics exposing (..)


prependIf : Bool -> a -> List a -> List a
prependIf shouldAdd item items =
    if shouldAdd then
        item :: items

    else
        items


lazyMap : (a -> b) -> (c -> a) -> (c -> b)
lazyMap applier original =
    \whatever -> applier (original whatever)


ifThenElse : Bool -> a -> a -> a
ifThenElse condition ifThen ifElse =
    if condition then
        ifThen

    else
        ifElse
