module UI.ListView exposing
    ( ListView, selectList, simpleList
    , ToggleableConfig, ToggleableCover, toggleableList
    , withItems, withSelect, withSelected, withDomId
    , SearchConfig, withSearchField, withActionBar, withSelectAllButton
    , withCustomExtraMenu, withHeader, withBadgedHeader
    , withWidth
    , SelectStyle, withSelectStyle
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

@docs SearchConfig, withSearchField, withActionBar, withSelectAllButton
@docs withCustomExtraMenu, withHeader, withBadgedHeader


## Width

@docs withWidth


## Select Style

@docs SelectStyle, withSelectStyle


# Rendering

@docs renderElement

-}

import Element exposing (Element, fill, px, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Keyed as Keyed
import UI.Badge as Badge exposing (Badge)
import UI.Button as Button
import UI.Icon as Icon
import UI.Internal.Basics exposing (maybeAnd, prependMaybe)
import UI.Internal.Clickable as Clickable
import UI.Internal.Colors as Colors
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Internal.Size as Size
import UI.Internal.ToggleableList as ToggleableList
import UI.Internal.Utils.Element as Element
import UI.Palette as Palette exposing (brightnessMiddle, tonePrimary)
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text
import UI.TextField as TextField
import UI.Utils.ARIA as ARIA
import UI.Utils.Action as Action
import UI.Utils.Element as Utils exposing (zeroPadding)


type alias Options object msg =
    { items : List object
    , searchField : Maybe (SearchConfig object msg)
    , actionBar : Maybe (Action.WithIcon msg)
    , select : Maybe (object -> msg)
    , isSelected : Maybe (object -> Bool)
    , width : Element.Length
    , selectStyle : SelectStyle
    , containerId : Maybe String
    , header : Maybe String
    , headerBadge : Maybe Badge
    , dropdown : Maybe (Dropdown msg)
    , selectAll : Maybe (SelectAll msg)
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
    ToggleableList.Config object msg


{-| What is displayed in a collapsed toggleable-list.

    { title = "Some item"
    , caption = Just "Created at 2020-06-10"
    }

-}
type alias ToggleableCover =
    ToggleableList.Cover


type alias Dropdown msg =
    { toggleMsg : msg, isEnabled : Bool, body : DropdownBody msg }


type DropdownBody msg
    = CustomDropdown (Element msg)



-- Constructor


{-| The main variation of `UI.ListView`.
Click an element, and it will be selected.

    ListView.selectList Msg.SelectElement
        (\{ name } ->
            Element.el [ Element.padding 8 ]
                (Element.text name)
        )

-}
selectList :
    (object -> msg)
    -> (object -> String)
    -> (Bool -> object -> Element msg)
    -> ListView object msg
selectList selectMsg toKey renderItem =
    simpleList toKey renderItem
        |> withSelect selectMsg


{-| A `UI.ListView` without built-in selection support.

    ListView.simpleList
        (\{ name } ->
            Element.el [ Element.padding 8 ]
                (Element.text name)
        )

-}
simpleList : (object -> String) -> (Bool -> object -> Element msg) -> ListView object msg
simpleList toKey renderItem =
    SelectList (Properties toKey (always renderItem))
        defaultOptions


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
                ToggleableList.selectedRow parentCfg config item

            else
                ToggleableList.defaultRow parentCfg config selected item
    in
    defaultOptions
        |> SelectList (Properties config.toKey toggleableItemView)
        |> withSelect config.selectMsg



-- Options


{-| Replaces the component's action-bar.
An action-bar is an additional pre-styled stick row that, when clicked, triggers an action.

    ListView.withActionBar
        { label = "Create new element"
        , icon = Icon.add
        , onClick =
            Msg.UuidGen (Msg.ForDialog << DialogMsg.OpenElementCreation)
        }
        someListView

-}
withActionBar :
    Action.WithIcon msg
    -> ListView object msg
    -> ListView object msg
withActionBar config (SelectList prop opt) =
    SelectList prop { opt | actionBar = Just config }


{-| Adds button to toggle a custom menu element.

    ListView.withCustomExtraMenu toggleMsg
        isMenuVisible
        menuBody
        someListView

-}
withCustomExtraMenu : msg -> Bool -> Element msg -> ListView object msg -> ListView object msg
withCustomExtraMenu toggleMsg isEnabled body (SelectList prop opt) =
    SelectList prop
        { opt
            | dropdown = Just <| Dropdown toggleMsg isEnabled (CustomDropdown body)
        }


{-| Adds a button to select every entry.

    ListView.withSelectAllButton Msg.SelectAll
        isEverythingSelected
        someListView

-}
withSelectAllButton : (Bool -> msg) -> Bool -> ListView object msg -> ListView object msg
withSelectAllButton message state (SelectList prop opt) =
    SelectList prop { opt | selectAll = Just <| SelectAll state message }


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


{-| Replaces the component's search-field and allow filtering the elements.

    ListView.withSearchFied
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
        someListView

-}
withSearchField :
    SearchConfig object msg
    -> ListView object msg
    -> ListView object msg
withSearchField options (SelectList prop opt) =
    { opt | searchField = Just options }
        |> SelectList prop


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


{-| Applies [`Element.width`](/packages/mdgriffith/elm-ui/latest/Element#width) to the component.

    ListView.withWidth
        (Element.fill |> Element.minimum 220)
        someListView

-}
withWidth : Element.Length -> ListView object msg -> ListView object msg
withWidth width (SelectList prop opt) =
    SelectList prop { opt | width = width }


{-| Overwrite the appearance of the selected item.

    ListView.withSelectStyle
        { backgroundColor = Palette.color toneDanger brightnessMiddle }
        someListView

-}
withSelectStyle : SelectStyle -> ListView object msg -> ListView object msg
withSelectStyle style (SelectList prop opt) =
    SelectList prop { opt | selectStyle = style }


{-| Add id attribute to the HTML tags of the elements and the list itself.

    ListView.selectList
        Msg.SelectDrink
        softDrinkToString
        softDrinkView
        |> ListView.withDomId "softDrinks"

Generates:

    < ... id="softDriks">
        <... id="fanta">...</...>
        <... id="coke">...</...>
        <... id="drPepper">...</...>
    </...>

**NOTE**: Only when `withDomId` is used children have `id`s.

-}
withDomId : String -> ListView object msg -> ListView object msg
withDomId containerId (SelectList prop opt) =
    SelectList prop { opt | containerId = Just containerId }


{-| Adds a header above the list.

    ListView.withHeader "ListView Header" someListView

-}
withHeader : String -> ListView object msg -> ListView object msg
withHeader header (SelectList prop opt) =
    SelectList prop { opt | header = Just header }


{-| Adds a header above the list, including a badge.

    ListView.withBadgedHeader "ListView Header"
        (Badge.primaryLight "NEW")
        someListView

-}
withBadgedHeader : String -> Badge -> ListView object msg -> ListView object msg
withBadgedHeader header badge (SelectList prop opt) =
    SelectList prop { opt | header = Just header, headerBadge = Just badge }



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
        , selectAllButtonView cfg opt.selectAll
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
                 , Border.color Colors.gray.light3
                 , Element.width fill
                 , Element.height fill
                 , Element.scrollbarY
                 ]
                    |> prependMaybe (Maybe.map Element.id opt.containerId)
                )
        , actionBarView cfg opt.actionBar
        ]



-- Internal


actionBarView : RenderConfig -> Maybe (Action.WithIcon msg) -> Element msg
actionBarView cfg actionBar =
    case actionBar of
        Just { label, icon, action } ->
            Element.row
                [ Element.width fill
                , Element.paddingEach
                    { bottom = 12
                    , left = 20
                    , right = 12
                    , top = 12
                    }
                , Background.color Colors.primary.light3
                , Font.color Colors.primary.middle
                ]
                [ Text.body2 label
                    |> Text.withColor (Palette.color tonePrimary brightnessMiddle)
                    |> Text.renderElement cfg
                , icon label
                    |> Icon.withSize Size.small
                    |> Icon.renderElement cfg
                    |> Element.el
                        [ Element.alignRight
                        ]
                ]
                |> Clickable.actionWrapElement cfg
                    [ Element.width fill ]
                    action

        Nothing ->
            Element.none


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


selectAllButtonView : RenderConfig -> Maybe (SelectAll msg) -> Element msg
selectAllButtonView cfg selectAll =
    case selectAll of
        Just { state, message } ->
            let
                color =
                    if state then
                        Palette.primaryDark1

                    else
                        Palette.primary

                checkAttrs =
                    Element.width (px 12)
                        :: Element.height (px 12)
                        :: Border.color (Palette.toElementColor color)
                        :: Border.width 1
                        :: Border.rounded 4
                        :: (ARIA.toElementAttributes <| ARIA.rolePresentation)

                checkIcon =
                    if state then
                        Element.el
                            (Background.color (Palette.toElementColor color) :: checkAttrs)
                            (selectAllCheck cfg)

                    else
                        Element.el
                            checkAttrs
                            Element.none

                rowAttrs =
                    Element.spacing 6
                        :: Element.paddingXY 8 6
                        :: Utils.onIndividualClick (message (not state))
                        :: Element.pointer
                        :: Background.color
                            (Palette.toElementColor <|
                                if state then
                                    Palette.grayLight2

                                else
                                    Palette.grayLight3
                            )
                        :: Element.mouseOver
                            [ Background.color <| Palette.toElementColor Palette.grayLight2 ]
                        :: Border.rounded 4
                        :: (ARIA.toElementAttributes <| ARIA.roleRadio state)
                        ++ Utils.colorTransition 100
            in
            Element.row rowAttrs
                [ Text.caption (cfg |> localeTerms >> .listView >> .selectAll)
                    |> Text.withColor color
                    |> Text.renderElement cfg
                , checkIcon
                ]
                |> Element.el
                    [ Element.paddingEach
                        { top = 0, bottom = 12, left = 12, right = 12 }
                    ]

        Nothing ->
            Element.none


selectAllCheck : RenderConfig -> Element msg
selectAllCheck cfg =
    (cfg |> localeTerms >> .listView >> .selectAll)
        |> Icon.check
        |> Icon.withCustomSize 10
        |> Icon.withColor
            (Palette.color
                Palette.tonePrimary
                Palette.brightnessMiddle
                |> Palette.setContrasting True
            )
        |> Icon.renderElement cfg
        |> Element.el
            [ Element.centerY
            , Element.centerX
            ]


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
                , dropdown cfg opt.dropdown
                ]

        Nothing ->
            Element.none


dropdown : RenderConfig -> Maybe (Dropdown msg) -> Element msg
dropdown cfg dropdownOptions =
    case dropdownOptions of
        Just opt ->
            let
                body =
                    if opt.isEnabled then
                        [ Element.below <| dropdownBody opt.body ]

                    else
                        []
            in
            (cfg |> localeTerms >> .sidebar >> .moreActions)
                |> Icon.moreActions
                |> Icon.withColor Palette.primary
                |> Icon.withSize Size.Small
                |> Button.fromIcon
                |> Button.cmd opt.toggleMsg Button.clear
                |> Button.withSize Size.small
                |> Button.renderElement cfg
                |> Element.el
                    (Element.alignRight
                        :: Element.pointer
                        :: Element.alignTop
                        :: body
                    )

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


dropdownBody : DropdownBody msg -> Element msg
dropdownBody body =
    case body of
        CustomDropdown html ->
            html


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
            , Border.color Colors.gray.light3
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
    , actionBar = Nothing
    , select = Nothing
    , isSelected = Nothing
    , width = Element.fill
    , selectStyle = defaultSelectStyle
    , containerId = Nothing
    , header = Nothing
    , headerBadge = Nothing
    , dropdown = Nothing
    , selectAll = Nothing
    }


defaultSelectStyle : SelectStyle
defaultSelectStyle =
    { backgroundColor = Just (Palette.color tonePrimary brightnessMiddle) }



-- Filter Internals


filterOptions : Maybe (SearchConfig object msg) -> List object -> List object
filterOptions searchOpt all =
    case Maybe.andThen .currentFilter searchOpt of
        Just ( value, filter ) ->
            List.filter (\obj -> filter value obj) all

        Nothing ->
            all
