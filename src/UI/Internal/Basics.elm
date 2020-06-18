module UI.Internal.Basics exposing (..)


prependIf : Bool -> a -> List a -> List a
prependIf shouldAdd item items =
    if shouldAdd then
        item :: items

    else
        items


prependMaybe : Maybe a -> List a -> List a
prependMaybe maybeSomething items =
    case maybeSomething of
        Just something ->
            something :: items

        Nothing ->
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


pairUncurry : (a -> b -> c) -> ( a, b ) -> c
pairUncurry applier ( first, second ) =
    applier first second


maybeToList : Maybe a -> List a
maybeToList maybeSomething =
    case maybeSomething of
        Just something ->
            [ something ]

        Nothing ->
            []
