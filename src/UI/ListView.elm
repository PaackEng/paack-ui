module UI.ListView exposing
    ( ListView, selectList
    , ToggleableConfig, ToggleableCover, toggleableList
    , withItems, withSelected
    , SearchConfig, withSearchField, ActionConfig, withActionBar
    , withWidth
    , renderElement
    )

{-| `UI.ListView` is a styled searchable row list.
The main variation (select-lists) has the capability of having one of its rows selected.

The developer is responsible for coding the row's view.
While this component then applies the borders and the click event.
Also, it can optionally filter when having a search bar, and add an action bar.

    view : RenderConfig -> Model -> Element Msg
    view renderConfig model =
        ListView.selectList Msg.SelectElement elementView
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
                , onClick =
                    Msg.UuidGen (Msg.ForDialog << DialogMsg.OpenElementCreation)
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

@docs ListView, selectList


# Toggleable variation

@docs ToggleableConfig, ToggleableCover, toggleableList


# Options

@docs withItems, withSelected


# Extra elements

@docs SearchConfig, withSearchField, ActionConfig, withActionBar


# Width

@docs withWidth


# Rendering

@docs renderElement

-}

import Element exposing (Element, fill)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Basics exposing (prependIf)
import UI.Internal.Palette as Palette
import UI.Internal.ToggleableList as ToggleableList
import UI.Palette as Palette exposing (brightnessMiddle, tonePrimary)
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text
import UI.TextField as TextField
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element


type alias Options object msg =
    { items : List object
    , searchField : Maybe (SearchConfig object msg)
    , actionBar : Maybe (ActionConfig msg)
    , isSelected : Maybe (object -> Bool)
    , width : Element.Length
    }


type alias Properties object msg =
    { select : object -> msg
    , renderItem : RenderConfig -> Bool -> object -> Element msg
    }


{-| The `ListView object msg` type is used for describing the component for later rendering.
-}
type ListView object msg
    = SelectList (Properties object msg) (Options object msg)


{-| `SearchConfig` assembles the required configuration for having a search field and filter.

    { label = "Search for elements matching name.."
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


{-| `ActionConfig` assembles the required configuration for having an action bar at the bottom of the list.

    { label = "Create new element"
    , icon = Icon.add
    , onClick =
        Msg.UuidGen (Msg.ForDialog << DialogMsg.OpenElementCreation)
    }

-}
type alias ActionConfig msg =
    { label : String
    , icon : String -> Icon
    , onClick : msg
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

    { title = "Some item", caption = "Created at 2020-06-10" }

-}
type alias ToggleableCover =
    ToggleableList.Cover



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
    -> (RenderConfig -> Bool -> object -> Element msg)
    -> ListView object msg
selectList selectMsg renderItem =
    SelectList (Properties selectMsg renderItem)
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
    selectList config.selectMsg toggleableItemView



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
    ActionConfig msg
    -> ListView object msg
    -> ListView object msg
withActionBar config (SelectList prop opt) =
    SelectList prop { opt | actionBar = Just config }


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



-- Render


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
        [ Border.widthEach { bottom = 0, left = 0, right = 1, top = 0 }
        , Border.color Palette.gray.lightest
        , Element.width opt.width
        , Element.height fill
        , Element.scrollbarY
        ]
        [ searchFieldView cfg opt.searchField
        , opt.items
            |> filterOptions opt.searchField
            |> List.map (\obj -> itemView cfg prop (isSelected obj) obj)
            |> Element.column
                [ Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
                , Border.color Palette.gray.lightest
                , Element.width fill
                , Element.height fill
                , Element.scrollbarY
                ]
        , actionBarView cfg opt.actionBar
        ]



-- Internal


actionBarView : RenderConfig -> Maybe (ActionConfig msg) -> Element msg
actionBarView cfg actionBar =
    case actionBar of
        Just { label, icon, onClick } ->
            Element.row
                (ARIA.toElementAttributes ARIA.roleButton
                    ++ [ Element.width fill
                       , Element.paddingEach
                            { bottom = 17
                            , left = 20
                            , right = 12
                            , top = 17
                            }
                       , Background.color Palette.primary.lightest
                       , Font.color Palette.primary.middle
                       , Element.pointer
                       , Events.onClick onClick
                       ]
                )
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

        Nothing ->
            Element.none


searchFieldView : RenderConfig -> Maybe (SearchConfig object msg) -> Element msg
searchFieldView cfg searchField =
    case searchField of
        Just { label, searchMsg, currentFilter } ->
            Element.row
                [ Element.width fill
                , Element.padding 12
                ]
                [ currentFilter
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault ""
                    |> TextField.search searchMsg label
                    |> TextField.withWidth TextField.widthFull
                    |> TextField.withPlaceholder label
                    |> TextField.withIcon (Icon.search "Search")
                    |> TextField.renderElement cfg
                ]

        Nothing ->
            Element.none


itemView : RenderConfig -> Properties object msg -> Bool -> object -> Element msg
itemView cfg { select, renderItem } selected obj =
    let
        selectedBg =
            Background.color Palette.primary.middle
    in
    Element.el
        ([ Events.onClick (select obj)
         , Element.pointer
         , Element.width fill
         , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
         , Border.color Palette.gray.lightest
         ]
            |> prependIf selected selectedBg
        )
        (renderItem cfg selected obj)


defaultOptions : Options object msg
defaultOptions =
    { items = []
    , searchField = Nothing
    , actionBar = Nothing
    , isSelected = Nothing
    , width = Element.fill
    }



-- Filter Internals


filterOptions : Maybe (SearchConfig object msg) -> List object -> List object
filterOptions searchOpt all =
    case Maybe.andThen .currentFilter searchOpt of
        Just ( value, filter ) ->
            List.filter (\obj -> filter value obj) all

        Nothing ->
            all
