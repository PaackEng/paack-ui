module UI.Internal.Basics exposing (..)


prependMaybe : Maybe a -> List a -> List a
prependMaybe maybeSomething items =
    case maybeSomething of
        Just something ->
            something :: items

        Nothing ->
            items


prependIf : Bool -> a -> List a -> List a
prependIf condition something items =
    if condition then
        something :: items

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


pairUncurry : (a -> b -> c) -> ( a, b ) -> c
pairUncurry applier ( first, second ) =
    applier first second


maybeNotThen : Maybe a -> Maybe a -> Maybe a
maybeNotThen replacement primary =
    case primary of
        Just sth ->
            Just sth

        Nothing ->
            replacement


maybeAnd : Bool -> Maybe a -> Maybe a
maybeAnd condition value =
    if condition then
        value

    else
        Nothing


flip : (a -> b -> c) -> b -> a -> c
flip applier b a =
    applier a b


maybeThen : (a -> b -> b) -> Maybe a -> b -> b
maybeThen applier maybeValue etc =
    case maybeValue of
        Just value ->
            applier value etc

        Nothing ->
            etc
