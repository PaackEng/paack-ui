module UI.Filter exposing
    ( Filter
    , FilterSize, withSize, sizeMedium, sizeExtraSmall, withWidth
    , renderElement
    , FilterModel, fromModel
    , singleTextFilter, multiTextFilter, singleDateFilter, rangeDateFilter, periodDateFilter, radioFilter
    , FilterMsg, update
    , setItems, getItems
    , FilterSorting, withSorting, sorting, withSortingPreview
    , FilterAppliedSorting, withAppliedSorting, sortingAscending, sortingDescending, notSorting
    , customFilter
    , withBody, withButtons, withBodyHeight
    , FilterAppliedHeader, withAppliedHeader, appliedHeader
    )

{-| The `UI.Filter` is a reusable dialog, hidden in a button, used for filtering the results of a list.


# Common

@docs Filter


## Size

@docs FilterSize, withSize, sizeMedium, sizeExtraSmall, withWidth


## Rendering

@docs renderElement


# Pre-defined filters

There are the predefined filters, which have states and updates, and includes sorting buttons.

    model =
        { someFilter =
            Filter.singleTextFilter Nothing .artist
                |> Filter.setItems [ example1, example2 ]
        , isFilterOpen = False
        }

    type Msg
        = FilterOpen
        | FilterClose
        | FilterEdit Filter.FilterMsg

    view renderConfig model =
        Filter.fromModel label
            Msg.FilterEdit
            model.someFilter
            |> Filter.renderElement renderConfig


## Builders

@docs FilterModel, fromModel
@docs singleTextFilter, multiTextFilter, singleDateFilter, rangeDateFilter, periodDateFilter, radioFilter
@docs FilterMsg, update
@docs setItems, getItems


### Sorting

    filterSorting model =
        Filter.sorting
            { sortAscendingMsg = Sort True
            , sortDescendingMsg = Sort False
            , clearSortingMsg = ClearSorting
            }
            |> Filter.withAppliedSorting Filter.sortingAscending
            |> Filter.withSortingPreview "A" "Z"

@docs FilterSorting, withSorting, sorting, withSortingPreview
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
            { openMsg = openMsg, closeMsg = closeMsg, isOpen = model.isFilterOpen }
            |> Filter.withSorting (filterSorting model)
            |> Filter.withBody (filterBody renderConfig model)
            |> Filter.withButtons (filterButtons renderConfig model)
            |> Filter.withAppliedHeader clearMsg labelWhenApplied
            |> Filter.renderElement renderConfig


## Builder

@docs customFilter


## Customizer

@docs withBody, withButtons, withBodyHeight


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


{-| Holds the filter's visual element information.
-}
type Filter msg
    = Filter (Internal.FullFilter msg)


{-| Describes the filter's sorting information.
-}
type FilterSorting msg
    = FilterSorting (Internal.FilterSorting msg)


{-| Describes the applied sorting state.
-}
type FilterAppliedSorting
    = FilterAppliedSorting (Maybe Sorter.SortingDirection)


{-| Holds a pre-defined filter's current state.
-}
type FilterModel msg item
    = FilterModel
        { sorting : Maybe (Sorter.Status item)
        , filter : Internal.Filter msg item
        , items : List item
        , result : List item
        , isOpen : Bool
        }


{-| Contains a pre-defined filter's change.
-}
type FilterMsg
    = FilterMsg Internal.Msg
    | SetSorting (Maybe Sorter.SortingDirection)
    | SetOpen Bool


{-| Describes the applied filter state.
-}
type FilterAppliedHeader msg
    = FilterAppliedHeader { preview : String, clearMsg : msg }


{-| Builds a custom filter, where you create the messages and treat the states.
-}
customFilter : String -> { openMsg : msg, closeMsg : msg, isOpen : Bool } -> Filter msg
customFilter label { openMsg, closeMsg, isOpen } =
    Filter
        { label = label
        , openMsg = openMsg
        , closeMsg = closeMsg
        , open = isOpen
        , width = Element.fill
        , size = Internal.Medium
        , sorting = Nothing
        , rows = always <| always []
        , rowsHeight = Nothing
        , buttons = always []
        , applied = Nothing
        }


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
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


{-| Scale the component between [the compatible sizes](#FilterSize).
-}
withSize : FilterSize -> Filter msg -> Filter msg
withSize (FilterSize newSize) (Filter filter) =
    Filter { filter | size = newSize }


{-| Sets the width with Element.
-}
withWidth : Element.Length -> Filter msg -> Filter msg
withWidth newWidth (Filter filter) =
    Filter { filter | width = newWidth }


{-| Sets the content of the filter's dialog when open.
-}
withBody : List (Element msg) -> Filter msg -> Filter msg
withBody newBody (Filter filter) =
    Filter { filter | rows = always <| always newBody }


{-| Sets the buttons at the bottom of the filter's dialog when open.
-}
withButtons : List (Button msg) -> Filter msg -> Filter msg
withButtons newButtons (Filter filter) =
    Filter { filter | buttons = always newButtons }


{-| Limits the vertical size of the filter's body
-}
withBodyHeight : Int -> Filter msg -> Filter msg
withBodyHeight maxPx (Filter filter) =
    Filter { filter | rowsHeight = Just maxPx }


{-| Sets the closed filter visually different when with filtering applied.
-}
withAppliedHeader : Maybe (FilterAppliedHeader msg) -> Filter msg -> Filter msg
withAppliedHeader maybeAppliedHeader (Filter filter) =
    Filter { filter | applied = Maybe.map (\(FilterAppliedHeader header) -> header) maybeAppliedHeader }


{-| Sets a preview of how many items (or which ones) were selected for filtering, with a button to fastly clear the filtering.
-}
appliedHeader : String -> msg -> FilterAppliedHeader msg
appliedHeader label clearMsg =
    FilterAppliedHeader { preview = label, clearMsg = clearMsg }


{-| A classic update function.
-}
update : FilterMsg -> FilterModel msg item -> ( FilterModel msg item, Effect msg )
update msg (FilterModel ({ filter, items } as model)) =
    case msg of
        FilterMsg subMsg ->
            let
                ( newFilter, outMsg ) =
                    Internal.update subMsg filter

                ( newResult, newOpen ) =
                    if outMsg.closeDialog then
                        ( Internal.apply newFilter model.sorting items
                        , False
                        )

                    else
                        ( model.result
                        , model.isOpen
                        )
            in
            ( FilterModel { model | filter = newFilter, result = newResult, isOpen = newOpen }
            , outMsg.effects
            )

        SetSorting newSortingDirection ->
            let
                newSorting =
                    Maybe.map
                        (Tuple.mapFirst (always newSortingDirection))
                        model.sorting
            in
            ( FilterModel { model | sorting = newSorting, result = Internal.apply filter newSorting items, isOpen = False }
            , Effect.none
            )

        SetOpen newState ->
            ( FilterModel { model | isOpen = newState }
            , Effect.none
            )


{-| Builds a [`FilterSorting`](#FilterSorting) for custom sorting.
-}
sorting : { sortAscendingMsg : msg, sortDescendingMsg : msg, clearSortingMsg : msg } -> FilterSorting msg
sorting { sortAscendingMsg, sortDescendingMsg, clearSortingMsg } =
    FilterSorting
        { preview = Nothing
        , sortAscendingMsg = sortAscendingMsg
        , sortDescendingMsg = sortDescendingMsg
        , clearSortingMsg = clearSortingMsg
        , applied = Nothing
        }


{-| For when the current sorting is ascending.
-}
sortingAscending : FilterAppliedSorting
sortingAscending =
    FilterAppliedSorting <| Just Sorter.SortAscending


{-| For when the current sorting is descending.
-}
sortingDescending : FilterAppliedSorting
sortingDescending =
    FilterAppliedSorting <| Just Sorter.SortDescending


{-| For when not sorting.
-}
notSorting : FilterAppliedSorting
notSorting =
    FilterAppliedSorting <| Nothing


{-| Adds sorting buttons to the filter.
-}
withSorting : FilterSorting msg -> Filter msg -> Filter msg
withSorting (FilterSorting sortingData) (Filter filter) =
    Filter { filter | sorting = Just sortingData }


{-| Sets the current sorting state.
-}
withAppliedSorting : FilterAppliedSorting -> FilterSorting msg -> FilterSorting msg
withAppliedSorting (FilterAppliedSorting appliedSorting) (FilterSorting sortingData) =
    FilterSorting { sortingData | applied = appliedSorting }


{-| Add example of the sorting order to the sorting buttons.
-}
withSortingPreview : { smaller : String, larger : String } -> FilterSorting msg -> FilterSorting msg
withSortingPreview preview (FilterSorting sortingData) =
    FilterSorting { sortingData | preview = Just preview }


{-| A pre-built, single text field filter.

    singleTextField (Just "initial filtering value") .someStringField

-}
singleTextFilter : Maybe String -> (item -> String) -> FilterModel msg item
singleTextFilter initialFilter getData =
    defaultFilterModel
        { sorting = Just ( Nothing, Sorter.AlphabeticalSortable getData )
        , filter = Internal.singleTextLocal initialFilter getData
        }


{-| A pre-built filter, with multiple text fields.

    multiTextField [] .someStringField

-}
multiTextFilter : List String -> (item -> String) -> FilterModel msg item
multiTextFilter initialList getData =
    defaultFilterModel
        { sorting = Just ( Nothing, Sorter.AlphabeticalSortable getData )
        , filter = Internal.multiTextLocal initialList getData
        }


{-| A pre-built filter, for a single date.

    singleDateFilter timeZone (Just model.someInitialTime) .someTimeField

-}
singleDateFilter : Time.Zone -> Maybe Time.Posix -> (item -> Time.Posix) -> FilterModel msg item
singleDateFilter timeZone initialTime getData =
    defaultFilterModel
        { sorting = Just ( Nothing, Sorter.IntegerSortable <| (getData >> Time.toMillis timeZone) )
        , filter = Internal.singleDateLocal timeZone initialTime getData
        }


{-| A pre-built filter, for a range of date.

    rangeDateFilter timeZone
        (Just model.someInitialBeginningTime)
        (Just model.someInitialEndingTime)
        .someTimeField

-}
rangeDateFilter : Time.Zone -> Maybe Time.Posix -> Maybe Time.Posix -> (item -> Time.Posix) -> FilterModel msg item
rangeDateFilter timeZone initialBegin initialEnd getData =
    defaultFilterModel
        { sorting = Just ( Nothing, Sorter.IntegerSortable <| (getData >> Time.toMillis timeZone) )
        , filter = Internal.rangeDateLocal timeZone initialBegin initialEnd getData
        }


{-| A pre-built filter, for a period (before, on, or after) some date.

    rangeDateFilter timeZone
        (Just model.someInitialBeginningTime)
        (Just model.someInitialEndingTime)
        .someTimeField

-}
periodDateFilter : Time.Zone -> Maybe Time.Posix -> Maybe Order -> (item -> Time.Posix) -> FilterModel msg item
periodDateFilter timeZone initialFilter initialComparer getData =
    defaultFilterModel
        { sorting = Just ( Nothing, Sorter.IntegerSortable <| (getData >> Time.toMillis timeZone) )
        , filter = Internal.periodDateLocal timeZone initialFilter (Maybe.map Internal.elmOrderToPeriodComparison initialComparer) getData
        }


{-| A pre-built filter, for a custom radio group.

    radioFilter [ "Orange", "Strawberry", "Pineapple", "Watermelon" ]
        (Just 0)
        (\fruit selected -> fruitsIndex fruit == selected)

-}
radioFilter : List String -> Maybe Int -> (item -> Int -> Bool) -> FilterModel msg item
radioFilter labelList initialSelection compare =
    defaultFilterModel
        { sorting = Nothing
        , filter = Internal.selectLocal labelList initialSelection compare
        }


{-| Feed the filter with items.
-}
setItems : List item -> FilterModel msg item -> FilterModel msg item
setItems items (FilterModel model) =
    FilterModel
        { model
            | items = items
            , result = Internal.apply model.filter model.sorting items
        }


{-| Retrieve items, sorted and filtered.
-}
getItems : FilterModel msg item -> List item
getItems (FilterModel { result }) =
    result


{-| Creates the visual component from the model of a pre-built filter.

    fromModel
        label
        Msg.ForFilter
        model.someFilter

-}
fromModel :
    String
    -> (FilterMsg -> msg)
    -> FilterModel msg item
    -> Filter msg
fromModel label toExternalMsg (FilterModel model) =
    Internal.defaultFilter
        { openMsg = toExternalMsg <| SetOpen True
        , closeMsg = toExternalMsg <| SetOpen False
        , editMsg = toExternalMsg << FilterMsg
        , sortAscendingMsg = toExternalMsg <| SetSorting <| Just Sorter.SortAscending
        , sortDescendingMsg = toExternalMsg <| SetSorting <| Just Sorter.SortDescending
        , clearSortingMsg = toExternalMsg <| SetSorting Nothing
        , label = label
        , isOpen = model.isOpen
        }
        model.filter
        model.sorting
        |> Filter


defaultFilterModel :
    { sorting : Maybe (Sorter.Status item), filter : Internal.Filter msg item }
    -> FilterModel msg item
defaultFilterModel init =
    FilterModel
        { sorting = init.sorting
        , filter = init.filter
        , items = []
        , result = []
        , isOpen = False
        }
