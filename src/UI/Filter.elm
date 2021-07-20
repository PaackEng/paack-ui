module UI.Filter exposing
    ( singleTextFilter, multiTextFilter, singleDateFilter, rangeDateFilter, periodDateFilter, radioFilter
    , EditDataMsg, filterUpdate
    , fromModel
    , withBody, withButtons
    , FilterSorting, withSorting, customSorting
    , Filter
    , FilterSize, withSize, sizeMedium, sizeExtraSmall
    , renderElement
    )

{-| The `UI.Filter` is a reusable dialog, hidden in a button, used for filtering the results of a list.


# Pre-defined filters

There are the predefined filters, which have states and updates, and includes sorting buttons.

    model =
        { data = []
        , someFilter = Filter.singleTextFilter Nothing .artist
        , isFilterOpen = False
        }

    type Msg
        = FilterOpen
        | FilterClose
        | FilterEdit Filter.EditDataMsg

    view renderConfig model =
        Filter.fromModel label
            { openMsg = openMsg
            , closeMsg = closeMsg
            , editMsg = FilterEdit
            }
            model.someFilter
            |> Filter.renderElement renderConfig model.isFilterOpen


## Builders

@docs singleTextFilter, multiTextFilter, singleDateFilter, rangeDateFilter, periodDateFilter, radioFilter
@docs EditDataMsg, filterUpdate
@docs fromModel


# Custom filters

There is also the possibility to create a custom filter, where you set the sorting, the filter fields/body, and the avaiable buttons.

    model =
        { etc | isFilterOpen = False }

    type Msg
        = Sort Bool
        | ClearSorting
        | Etc -- ...

    view renderConfig model =
        Filter.customFilter label
            { openMsg = openMsg, closeMsg = closeMsg }
            |> Filter.withSorting (filterSorting model)
            |> Filter.withBody (filterBody renderConfig model)
            |> Filter.withButtons (filterButtons renderConfig model)
            |> Filter.withAppliedHeader clearMsg labelWhenApplied
            |> Filter.renderElement renderConfig

    filterSorting model =
        Filter.customSorting
            { ascendingMsg = Sort True
            , descendingMsg = Sort False
            , clearMsg = ClearSorting
            , applied = model.sortingDirection
            }
            |> Filter.sortingWithPreview "A" "Z"


## Builder

@docs customFilter


## Customizer

@docs withBody, withButtons, withAppliedHeader


### Sorting

@docs FilterSorting, withSorting, customSorting


# Common

@docs Filter


## Size

@docs FilterSize, withSize, sizeMedium, sizeExtraSmall


## Rendering

@docs renderElement

-}


{-| Describes a compatible size.
-}
type FilterSize
    = FilterSize Internal.FilterSize


{-| Smallest size.
-}
sizeExtraSmall : FilterSize
sizeExtraSmall =
    FilterSize Internal.ExtraSmall


{-| Default size.
-}
sizeMedium : FilterSize
sizeMedium =
    FilterSize Internal.Medium
