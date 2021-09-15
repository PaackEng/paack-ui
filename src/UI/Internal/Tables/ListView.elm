module UI.Internal.Tables.ListView exposing
    ( toggleableList
    , withItems, withSelected
    , renderElement
    )

{-| `UI.ListView` is a styled searchable row list.
The main variation (select-lists) has the capability of having one of its rows selected.

The developer is responsible for coding the row's view.
While this component then applies the borders and the click event.
Also, it can optionally filter when having a search bar, and add an action bar.

    view : RenderConfig -> Model -> Element Msg
    view renderConfig model =
        ListView.selectList Msg.SelectElement
            elementToKey
            elementView
            |> ListView.withItems model.myListElements
            |> ListView.withSearchField
                { label = "Search for elements matching name.."
                , searchMsg = Msg.FilterSet
                , currentFilter =
                    Maybe.map
                        (\str -> ( str, elementHasString ))
                        model.currentFilter
                }
            |> ListView.withActionBar
                { label = "Create new element"
                , icon = Icon.add
                , action =
                    DialogMsg.OpenElementCreation
                        |> Msg.ForDialog
                        |> Action.DispatchMsg
                }
            |> ListView.withSelected
                (\{ id } ->
                    Maybe.map (.id >> (==) id) model.selectedElement
                        |> Maybe.withDefault False
                )
            |> ListView.withWidth Element.fill
            |> ListView.renderElement renderConfig

    elementHasString : String -> Element -> Bool
    elementHasString str { name } =
        String.contains str name


# Building

@docs ListView, selectList, simpleList


# Toggleable variation

@docs ToggleableConfig, ToggleableCover, toggleableList


# Options

@docs withItems, withSelect, withSelected, withDomId


## Extra elements

@docs SearchConfig, withSearchField, withActionBar
@docs withBottomButtonsRow, withBottomButtonsColumn, withSelectAllButton
@docs withFilter, withCustomExtraMenu, withHeader, withBadgedHeader


## Width

@docs withWidth


## Select Style

@docs SelectStyle, withSelectStyle


# Rendering

@docs renderElement

-}

import Element exposing (Attribute, Element, fill, px, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Keyed as Keyed
import UI.Badge as Badge exposing (Badge)
import UI.Checkbox as Checkbox
import UI.Filter as Filter exposing (Filter)
import UI.Icon as Icon
import UI.Internal.Basics exposing (ifThenElse, maybeAnd, prependMaybe)
import UI.Internal.Colors as Colors
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Internal.Utils.Element as Element
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text exposing (ellipsize)
import UI.TextField as TextField
import UI.Utils.Element exposing (zeroPadding)


type alias Options object msg =
    { items : List object
    , searchField : Maybe (SearchConfig object msg)
    , select : Maybe (object -> msg)
    , isSelected : Maybe (object -> Bool)
    , width : Element.Length
    , selectStyle : SelectStyle
    , containerId : Maybe String
    , header : Maybe String
    , headerBadge : Maybe Badge
    , selectAll : Maybe (SelectAll msg)
    , filter : Maybe (Filter msg)
    }


type alias Properties object msg =
    { toKey : object -> String
    , renderItem : RenderConfig -> Bool -> object -> Element msg
    }


{-| The `ListView object msg` type is used for describing the component for later rendering.
-}
type ListView object msg
    = SelectList (Properties object msg) (Options object msg)


{-| `SearchConfig` assembles the required configuration for having a search field and filter.

    { title = "Elements"
    , label = "Search for elements matching name.."
    , searchMsg = Msg.FilterSet
    , currentFilter =
        Maybe.map
            (\str -> ( str, elementHasString ))
            model.currentFilter
    }

-}
type alias SearchConfig object msg =
    { label : String
    , searchMsg : String -> msg
    , currentFilter : Maybe ( String, String -> object -> Bool )
    }


{-| The selected item can be styled using [`ListView.withSelectStyle`](#withSelectStyle).

  - `backgroundColor`: The color in which the background assumes for selected items.
    When `Nothing` no color is applied.

-}
type alias SelectStyle =
    { backgroundColor : Maybe Palette.Color
    }



-- Expose


{-| Configuration required to render a toggleable-list.

    { detailsShowLabel = "Show details" -- For accessibility only
    , detailsCollapseLabel = "Hide details" -- For accessibility only
    , toCover =
        \{ name } ->
            -- ListView.ToggleableCover
            { title = name, caption = Nothing }
    , toDetails =
        \{ age } ->
            [ ( "Age", Element.text age ) ]
    , selectMsg = Msg.ElementSelect
    }

-}
type alias ToggleableConfig object msg =
    { detailsShowLabel : String
    , detailsCollapseLabel : String
    , toCover : object -> Cover
    , toDetails : object -> List ( String, Element msg )
    , selectMsg : object -> msg
    , toKey : object -> String
    }


type alias Cover =
    { title : String, caption : Maybe String }



-- Constructor


{-| Toggleable-lists are a variation of select-lists where the selected element expands with details while all other's details keep collapsed.

We recommend using `UI.Table` instead, as it uses toggleable-lists for its responsive mode.

    ListView.toggleableList
        { detailsShowLabel = "Show details" -- For accessibility only
        , detailsCollapseLabel = "Hide details" -- For accessibility only
        , toCover =
            \{ name } ->
                { title = name, caption = Nothing }
        , toDetails =
            \{ age } ->
                [ ( "Age", Element.text age ) ]
        , selectMsg = Msg.ElementSelect
        }
        |> ListView.withItems model.items
        |> ListView.withSelected isElementSelected
        |> ListView.renderElement renderConfig

**NOTE**: Toggleable-list elements' view is not codable.

-}
toggleableList : ToggleableConfig object msg -> ListView object msg
toggleableList config =
    let
        toggleableItemView parentCfg selected item =
            if selected then
                selectedRow parentCfg config item

            else
                defaultRow parentCfg config selected item
    in
    defaultOptions
        |> SelectList (Properties config.toKey toggleableItemView)
        |> withSelect config.selectMsg


defaultRow : RenderConfig -> ToggleableConfig object msg -> Bool -> object -> Element msg
defaultRow renderConfig config selected object =
    Element.row
        [ Element.width fill
        , Element.paddingEach { top = 11, bottom = 11, left = 28, right = 12 }
        , Element.height Element.shrink
        ]
        [ coverView renderConfig (config.toCover object) selected
            |> Element.el
                [ Element.width fill
                , Element.centerY
                , Element.clipX
                , Element.paddingXY 0 5
                ]
        , ifThenElse selected
            (Icon.toggleUp config.detailsCollapseLabel)
            (Icon.toggleDown config.detailsShowLabel)
            |> Icon.withSize Size.large
            |> Icon.withColor (titleColor selected)
            |> Icon.renderElement renderConfig
            |> Element.el
                [ Font.center
                , Element.width (px 32)
                , Element.paddingXY 0 6
                , Element.centerY
                ]
        ]


selectedRow : RenderConfig -> ToggleableConfig object msg -> object -> Element msg
selectedRow renderConfig config object =
    Element.column [ Element.width fill ]
        [ defaultRow renderConfig config True object
        , object
            |> config.toDetails
            |> List.map (detailItem renderConfig)
            |> Keyed.column toggleableCard
        ]



-- Options


{-| Replaces the component's list of elements.

    ListView.withItems
        [ { id = 0, name = "Catarina" }
        , { id = 1, name = "Gabriel" }
        ]
        someListView

-}
withItems : List object -> ListView object msg -> ListView object msg
withItems items (SelectList prop opt) =
    SelectList prop { opt | items = items }


{-| Adds a message to be dispatched when an item is selected.

    ListView.withItems Msg.ElementSelect
        someListView

-}
withSelect : (object -> msg) -> ListView object msg -> ListView object msg
withSelect options (SelectList prop opt) =
    { opt | select = Just options }
        |> SelectList prop


{-| Marks every element as selected or not using a boolean-resulted lambda.

    ListView.withSelected
        (\{ id } -> id == model.selectedId)
        someListView

-}
withSelected : (object -> Bool) -> ListView object msg -> ListView object msg
withSelected isSelected (SelectList prop opt) =
    SelectList prop { opt | isSelected = Just isSelected }



-- Render


type alias SelectAll msg =
    { state : Bool
    , message : Bool -> msg
    }


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> ListView object msg -> Element msg
renderElement cfg (SelectList prop opt) =
    let
        isSelected obj =
            case opt.isSelected of
                Just ask ->
                    ask obj

                Nothing ->
                    False
    in
    Element.column
        [ Element.width opt.width
        , Element.height fill
        , Element.scrollbarY
        ]
        [ searchFieldView cfg opt
        , toolbarView cfg opt
        , opt.items
            |> filterOptions opt.searchField
            |> List.map
                (\obj ->
                    itemView cfg
                        prop
                        opt
                        opt.selectStyle.backgroundColor
                        (isSelected obj)
                        obj
                )
            |> Keyed.column
                ([ Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
                 , Border.color Colors.gray200
                 , Element.width fill
                 , Element.height fill
                 , Element.scrollbarY
                 ]
                    |> prependMaybe (Maybe.map Element.id opt.containerId)
                )
        ]



-- Internal


searchFieldView : RenderConfig -> Options object msg -> Element msg
searchFieldView cfg opt =
    case opt.searchField of
        Just { label, searchMsg, currentFilter } ->
            Element.column
                [ Element.width fill
                , Element.padding 12
                ]
                [ headerView cfg opt
                , currentFilter
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault ""
                    |> TextField.search searchMsg label
                    |> TextField.withWidth TextField.widthFull
                    |> TextField.withPlaceholder label
                    |> TextField.withIcon
                        (cfg |> localeTerms >> .listView >> .search |> Icon.search)
                    |> TextField.renderElement cfg
                ]

        Nothing ->
            Element.none


toolbarView : RenderConfig -> Options object msg -> Element msg
toolbarView cfg opt =
    let
        entries =
            []
                |> prependMaybe
                    (Maybe.map (filterView cfg) opt.filter)
                |> prependMaybe
                    (Maybe.map (selectAllButtonView cfg) opt.selectAll)
    in
    if List.isEmpty entries then
        Element.none

    else
        Element.row
            [ Element.width Element.fill
            , Element.paddingEach
                { bottom = 12
                , left = 15
                , right = 15
                , top = 0
                }
            ]
            entries


selectAllButtonView : RenderConfig -> SelectAll msg -> Element msg
selectAllButtonView cfg { state, message } =
    Checkbox.checkbox (cfg |> localeTerms >> .listView >> .selectAll) message state
        |> Checkbox.renderElement cfg
        |> Element.el [ Element.width fill, Element.alignTop ]


filterView : RenderConfig -> Filter msg -> Element msg
filterView cfg filter =
    filter
        |> Filter.withSize Filter.sizeExtraSmall
        |> Filter.withAlignRight
        |> Filter.renderElement cfg
        |> Element.el [ Element.width shrink, Element.alignRight, Element.alignTop ]


headerView : RenderConfig -> Options object msg -> Element msg
headerView cfg opt =
    case opt.header of
        Just header ->
            Element.row
                [ Element.width fill
                , Element.height fill
                , Element.paddingXY 0 12
                , Element.spacing 8
                ]
                [ Text.heading5 header
                    |> Text.renderElement cfg
                , headerBadge cfg opt
                ]

        Nothing ->
            Element.none


headerBadge : RenderConfig -> Options object msg -> Element msg
headerBadge cfg opt =
    case opt.headerBadge of
        Just badge ->
            badge
                |> Badge.renderElement cfg
                |> Element.el [ Element.centerY ]

        Nothing ->
            Element.none


itemView :
    RenderConfig
    -> Properties object msg
    -> Options object msg
    -> Maybe Palette.Color
    -> Bool
    -> object
    -> ( String, Element msg )
itemView cfg { renderItem, toKey } { select, containerId } background selected obj =
    let
        key =
            toKey obj

        attributes =
            [ Element.pointer
            , Element.width fill
            , Border.widthEach { zeroPadding | bottom = 1 }
            , Border.color Colors.gray200
            ]
                |> prependMaybe (Maybe.map ((|>) obj >> Events.onClick) select)
                |> prependMaybe
                    (background
                        |> Maybe.map (Palette.toElementColor >> Background.color)
                        |> maybeAnd selected
                    )
                |> prependMaybe
                    (Maybe.map (always <| Element.id key) containerId)
    in
    ( key
    , Element.el
        attributes
        (renderItem cfg selected obj)
    )


defaultOptions : Options object msg
defaultOptions =
    { items = []
    , searchField = Nothing
    , select = Nothing
    , isSelected = Nothing
    , width = Element.fill
    , selectStyle = defaultSelectStyle
    , containerId = Nothing
    , header = Nothing
    , headerBadge = Nothing
    , selectAll = Nothing
    , filter = Nothing
    }


defaultSelectStyle : SelectStyle
defaultSelectStyle =
    { backgroundColor = Just Palette.blue700 }



-- Filter Internals


filterOptions : Maybe (SearchConfig object msg) -> List object -> List object
filterOptions searchOpt all =
    case Maybe.andThen .currentFilter searchOpt of
        Just ( value, filter ) ->
            List.filter (\obj -> filter value obj) all

        Nothing ->
            all


titleColor : Bool -> Palette.Color
titleColor selected =
    if selected then
        Palette.genericWhite

    else
        Palette.gray800


coverView : RenderConfig -> Cover -> Bool -> Element msg
coverView cfg { title, caption } selected =
    let
        titleComponent =
            Text.body1 title
                |> Text.withColor (titleColor selected)

        captionApplied =
            case caption of
                Just captionStr ->
                    Text.combination
                        [ titleComponent
                        , Text.caption captionStr
                            |> Text.withColor (captionColor selected)
                        ]

                Nothing ->
                    titleComponent
    in
    captionApplied
        |> Text.withOverflow ellipsize
        |> Text.renderElement cfg
        |> Element.el [ Element.width fill, Element.clipX ]


toggleableCard : List (Attribute msg)
toggleableCard =
    [ Element.paddingEach { top = 16, bottom = 19, left = 28, right = 20 }
    , Palette.gray200
        |> Palette.toElementColor
        |> Background.color
    , Element.width fill
    , Element.spacing 12
    ]


detailItem : RenderConfig -> ( String, Element msg ) -> ( String, Element msg )
detailItem renderConfig ( label, content ) =
    ( label
    , Element.column indentedDetailItemAttributes
        [ label
            |> Text.overline
            |> Text.withColor Palette.gray600
            |> Text.withOverflow ellipsize
            |> Text.renderElement renderConfig
        , content
        ]
    )


indentedDetailItemAttributes : List (Attribute msg)
indentedDetailItemAttributes =
    [ Element.paddingEach { zeroPadding | left = 8 }
    , Border.widthEach { zeroPadding | left = 2 }
    , Palette.blue700
        |> Palette.toElementColor
        |> Border.color
    , Element.width fill
    ]


captionColor : Bool -> Palette.Color
captionColor selected =
    if selected then
        Palette.blue400

    else
        Palette.gray600
