module UI.Utils.TypeNumbers exposing
    ( Zero, Increase
    , One, Two, Three, Four, Five, Six, Seven, Eight, Nine
    , Decimal
    )

{-| This module contains phantom-types for helping guarantee lengths in collectibles.

The UI.Table uses this for constraining all rows dimensions.

    type alias CoolTable columns =
        { header : NList String columns
        , rows : NLIst Int columns
        }


# Primitives

@docs Zero, Increase


# Descendents

@docs One, Two, Three, Four, Five, Six, Seven, Eight, Nine
@docs Decimal

-}


{-| Represents the size of an empty set.
-}
type alias Zero =
    Never


{-| Represents an recursively incremented size.

    pushItem : Items n -> Items (Increase n)

    popItem : Items (Increase n) -> Items n

-}
type Increase a
    = Increase a


{-| Equivalent to `(Increase Zero)`
-}
type alias One =
    Increase Zero


{-| Equivalent to `(Increase (Increase Zero))`
-}
type alias Two =
    Increase One


{-| Equivalent to `(Increase (Increase (Increase Zero)))`
-}
type alias Three =
    Increase Two


{-| Equivalent to `(Increase Three)`
-}
type alias Four =
    Increase Three


{-| Equivalent to `(Increase Four)`
-}
type alias Five =
    Increase Four


{-| Equivalent to `(Increase Five)`
-}
type alias Six =
    Increase Five


{-| Equivalent to `(Increase Six)`
-}
type alias Seven =
    Increase Six


{-| Equivalent to `(Increase Seven)`
-}
type alias Eight =
    Increase Seven


{-| Equivalent to `(Increase Eight)`
-}
type alias Nine =
    Increase Eight


{-| Used to compose numbers greater than nine.

`(Decimal (Decimal Zero))` is equivalent to 20.

-}
type alias Decimal base =
    Increase (Increase (Increase (Increase (Increase (Increase (Increase (Increase (Increase (Increase base)))))))))
