module UI.Filter exposing
    ( Filter
    , FilterSize, withSize, sizeMedium, sizeExtraSmall
    , renderElement
    , FilterModel, fromModel
    , singleTextFilter, multiTextFilter, singleDateFilter, rangeDateFilter, periodDateFilter, radioFilter
    , FilterMsg, filterUpdate
    , setItems, getItems
    , FilterSorting, withSorting, sorting
    , FilterAppliedSorting, withAppliedSorting, sortingAscending, sortingDescending, notSorting
    , customFilter
    , withBody, withButtons
    , FilterAppliedHeader, withAppliedHeader, appliedHeader
    )

{-| The `UI.Filter` is a reusable dialog, hidden in a button, used for filtering the results of a list.


# Common

@docs Filter


## Size

@docs FilterSize, withSize, sizeMedium, sizeExtraSmall


## Rendering

@docs renderElement


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
        | FilterEdit Filter.FilterMsg

    view renderConfig model =
        Filter.fromModel label
            { openMsg = openMsg
            , closeMsg = closeMsg
            , editMsg = Msg.FilterEdit
            , isOpen = model.isFilterOpen
            }
            model.someFilter
            |> Filter.withSorting (filterSorting model)
            |> Filter.renderElement renderConfig


## Builders

@docs FilterModel, fromModel
@docs singleTextFilter, multiTextFilter, singleDateFilter, rangeDateFilter, periodDateFilter, radioFilter
@docs FilterMsg, filterUpdate
@docs setItems, getItems


### Sorting

    filterSorting model =
        Filter.sorting
            { ascendingMsg = Sort True
            , descendingMsg = Sort False
            , clearMsg = ClearSorting
            }
            |> Filter.withAppliedSorting Filter.sortingAscending
            |> Filter.sortingWithPreview "A" "Z"

@docs FilterSorting, withSorting, sorting
@docs FilterAppliedSorting, withAppliedSorting, sortingAscending, sortingDescending, notSorting


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


## Builder

@docs customFilter


## Customizer

@docs withBody, withButtons


## Header when filter applied

@docs FilterAppliedHeader, withAppliedHeader, appliedHeader

-}

import Element exposing (Element)
import Time
import UI.Button exposing (Button)
import UI.Effect as Effect exposing (Effect)
import UI.Internal.Filter.Apply as Internal
import UI.Internal.Filter.Model as Internal
import UI.Internal.Filter.Msg as Internal
import UI.Internal.Filter.Sorter as Sorter
import UI.Internal.Filter.Update as Internal
import UI.Internal.Filter.View as Internal
import UI.RenderConfig exposing (RenderConfig)


type Filter msg
    = Filter (Internal.FullFilter msg)


type FilterSorting msg
    = FilterSorting (Internal.FilterSorting msg)


type FilterAppliedSorting
    = FilterAppliedSorting (Maybe Sorter.SortingDirection)


type FilterModel msg item
    = FilterModel
        { sorting : Maybe (Sorter.Status item)
        , filter : Internal.Filter msg item
        , items : List item
        , result : List item
        }


type FilterMsg
    = FilterMsg Internal.Msg
    | SetSorting (Maybe Sorter.SortingDirection)


type FilterAppliedHeader msg
    = FilterAppliedHeader { preview : String, clearMsg : msg }


customFilter : String -> { openMsg : msg, closeMsg : msg } -> Filter msg
customFilter label { openMsg, closeMsg } =
    Filter
        { label = label
        , openMsg = openMsg
        , closeMsg = closeMsg
        , open = False
        , width = Element.fill
        , size = Internal.Medium
        , sorting = Nothing
        , rows = always <| always []
        , buttons = always []
        , applied = Nothing
        }


renderElement : RenderConfig -> Filter msg -> Element msg
renderElement renderConfig (Filter filter) =
    Internal.renderElement renderConfig filter


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


withSize : FilterSize -> Filter msg -> Filter msg
withSize (FilterSize newSize) (Filter filter) =
    Filter { filter | size = newSize }


withWidth : Element.Length -> Filter msg -> Filter msg
withWidth newWidth (Filter filter) =
    Filter { filter | width = newWidth }


withBody : List (Element msg) -> Filter msg -> Filter msg
withBody newBody (Filter filter) =
    Filter { filter | rows = always <| always newBody }


withButtons : List (Button msg) -> Filter msg -> Filter msg
withButtons newButtons (Filter filter) =
    Filter { filter | buttons = always newButtons }


withAppliedHeader : Maybe (FilterAppliedHeader msg) -> Filter msg -> Filter msg
withAppliedHeader maybeAppliedHeader (Filter filter) =
    Filter { filter | applied = Maybe.map (\(FilterAppliedHeader header) -> header) maybeAppliedHeader }


appliedHeader : String -> msg -> FilterAppliedHeader msg
appliedHeader label clearMsg =
    FilterAppliedHeader { preview = label, clearMsg = clearMsg }


filterUpdate : FilterMsg -> FilterModel msg item -> ( FilterModel msg item, Effect msg )
filterUpdate msg (FilterModel ({ filter, items } as model)) =
    case msg of
        FilterMsg subMsg ->
            let
                ( newFilter, outMsg ) =
                    Internal.update subMsg filter

                newResult =
                    if outMsg.closeDialog then
                        Internal.apply newFilter model.sorting items

                    else
                        model.result
            in
            ( FilterModel { model | filter = newFilter, result = newResult }
            , outMsg.effects
            )

        SetSorting newSortingDirection ->
            let
                newSorting =
                    Maybe.map
                        (Tuple.mapFirst (always newSortingDirection))
                        model.sorting
            in
            ( FilterModel { model | sorting = newSorting, result = Internal.apply filter newSorting items }
            , Effect.none
            )


sorting : { sortAscendingMsg : msg, sortDescendingMsg : msg, clearSortingMsg : msg } -> FilterSorting msg
sorting { sortAscendingMsg, sortDescendingMsg, clearSortingMsg } =
    FilterSorting
        { preview = Nothing
        , sortAscendingMsg = sortAscendingMsg
        , sortDescendingMsg = sortDescendingMsg
        , clearSortingMsg = clearSortingMsg
        , applied = Nothing
        }


sortingAscending : FilterAppliedSorting
sortingAscending =
    FilterAppliedSorting <| Just Sorter.SortAscending


sortingDescending : FilterAppliedSorting
sortingDescending =
    FilterAppliedSorting <| Just Sorter.SortDescending


notSorting : FilterAppliedSorting
notSorting =
    FilterAppliedSorting <| Nothing


withSorting : FilterSorting msg -> Filter msg -> Filter msg
withSorting (FilterSorting sortingData) (Filter filter) =
    Filter { filter | sorting = Just sortingData }


withAppliedSorting : FilterAppliedSorting -> FilterSorting msg -> FilterSorting msg
withAppliedSorting (FilterAppliedSorting appliedSorting) (FilterSorting sortingData) =
    FilterSorting { sortingData | applied = appliedSorting }


singleTextFilter : Maybe String -> (item -> String) -> FilterModel msg item
singleTextFilter initialFilter getData =
    FilterModel
        { sorting = Just ( Nothing, Sorter.AlphabeticalSortable getData )
        , filter = Internal.singleTextLocal initialFilter getData
        , items = []
        , result = []
        }


multiTextFilter : List String -> (item -> String) -> FilterModel msg item
multiTextFilter initialList getData =
    FilterModel
        { sorting = Just ( Nothing, Sorter.AlphabeticalSortable getData )
        , filter = Internal.multiTextLocal initialList getData
        , items = []
        , result = []
        }


singleDateFilter : Time.Zone -> Maybe Time.Posix -> (item -> Time.Posix) -> FilterModel msg item
singleDateFilter timeZone initialTime getData =
    FilterModel
        { sorting = Just ( Nothing, Sorter.IntegerSortable <| (getData >> Time.toMillis timeZone) )
        , filter = Internal.singleDateLocal timeZone initialTime getData
        , items = []
        , result = []
        }


rangeDateFilter : Time.Zone -> Maybe Time.Posix -> Maybe Time.Posix -> (item -> Time.Posix) -> FilterModel msg item
rangeDateFilter timeZone initialBegin initialEnd getData =
    FilterModel
        { sorting = Just ( Nothing, Sorter.IntegerSortable <| (getData >> Time.toMillis timeZone) )
        , filter = Internal.rangeDateLocal timeZone initialBegin initialEnd getData
        , items = []
        , result = []
        }


periodDateFilter : Time.Zone -> Maybe Time.Posix -> Maybe Order -> (item -> Time.Posix) -> FilterModel msg item
periodDateFilter timeZone initialFilter initialComparer getData =
    FilterModel
        { sorting = Just ( Nothing, Sorter.IntegerSortable <| (getData >> Time.toMillis timeZone) )
        , filter = Internal.periodDateLocal timeZone initialFilter (Maybe.map Internal.elmOrderToPeriodComparison initialComparer) getData
        , items = []
        , result = []
        }


radioFilter : List String -> Maybe Int -> (item -> Int -> Bool) -> FilterModel msg item
radioFilter labelList initialSelection compare =
    FilterModel
        { sorting = Nothing
        , filter = Internal.selectLocal labelList initialSelection compare
        , items = []
        , result = []
        }


setItems : List item -> FilterModel msg item -> FilterModel msg item
setItems items (FilterModel model) =
    FilterModel
        { model
            | items = items
            , result = Internal.apply model.filter model.sorting items
        }


getItems : FilterModel msg item -> List item
getItems (FilterModel { result }) =
    result


fromModel :
    String
    ->
        { openMsg : msg
        , closeMsg : msg
        , toExternalMsg : FilterMsg -> msg
        , isOpen : Bool
        }
    -> FilterModel msg item
    -> Filter msg
fromModel label { openMsg, closeMsg, toExternalMsg, isOpen } (FilterModel model) =
    Internal.defaultFilter
        { openMsg = openMsg
        , closeMsg = closeMsg
        , editMsg = toExternalMsg << FilterMsg
        , sortAscendingMsg = toExternalMsg <| SetSorting <| Just Sorter.SortAscending
        , sortDescendingMsg = toExternalMsg <| SetSorting <| Just Sorter.SortDescending
        , clearSortingMsg = toExternalMsg <| SetSorting Nothing
        , label = label
        , isOpen = isOpen
        }
        model.filter
        model.sorting
        |> Filter
