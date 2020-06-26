module UI.RowList exposing
    ( RowList, selectList
    , ToggleableConfig, ToggleableCover, toggleableList
    , withOptions, withSelected
    , withSearchField, withActionBar
    , withWidth
    , renderElement
    )

{-| `UI.RowList` is a styled searchable row list.
The main variation (select-lists) has the capability of having one of its rows selected.

The developer does the rendering of the row elements.
While this component then applies the borders and the click event.
Also, it can optionally filter when having a search bar, and add an action bar.

    view : RenderConfig -> Model -> Element Msg
    view renderConfig model =
        RowList.selectList Msg.SelectElement elementView
            |> RowList.withOptions model.myListElements
            |> RowList.withSearchField
                "Search for elements matching name.."
                FilterSet
                (Maybe.map (\str -> ( str, elementHasString )) model.currentFilter)
            |> RowList.withActionBar "Create new Element"
                Icon.add
                (Msg.UuidGen (Msg.ForDialog << DialogMsg.OpenElementCreation))
            |> RowList.withSelected
                (\{ id } ->
                    Maybe.map (.id >> (==) id) model.selectedElement
                        |> Maybe.withDefault False
                )
            |> RowList.withWidth Element.fill
            |> RowList.renderElement renderConfig

    elementHasString : String -> Element -> Bool
    elementHasString str { name } =
        String.contains str name


# Building

@docs RowList, selectList


# Toggleable variation

Toggleable-lists are a variation of select-lists where the selected element expands with

@docs ToggleableConfig, ToggleableCover, toggleableList


# Options

@docs withOptions, withSelected


# Extra elements

@docs withSearchField, withActionBar


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


type alias SearchOptions object msg =
    { label : String
    , searchMsg : String -> msg
    , currentFilter : Maybe ( String, String -> object -> Bool )
    }


type alias Options object msg =
    -- FUTURE: sort: Maybe (List object -> List object)
    { items : List object
    , searchField : Maybe (SearchOptions object msg)
    , actionBar : Maybe ( String, String -> Icon, msg )
    , isSelected : Maybe (object -> Bool)
    , width : Element.Length
    }


type alias Properties object msg =
    { select : object -> msg
    , renderItem : RenderConfig -> Bool -> object -> Element msg
    }


{-| The `RowList object msg` type is used for describing the component for later rendering.
-}
type RowList object msg
    = SelectList (Properties object msg) (Options object msg)



-- Expose


{-| -}
type alias ToggleableConfig object msg =
    ToggleableList.Config object msg


type alias ToggleableCover =
    ToggleableList.Cover



-- Constructor


selectList : (object -> msg) -> (RenderConfig -> Bool -> object -> Element msg) -> RowList object msg
selectList selectMsg renderItem =
    SelectList (Properties selectMsg renderItem)
        defaultOptions


toggleableList : ToggleableConfig object msg -> RowList object msg
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


withActionBar : String -> (String -> Icon) -> msg -> RowList object msg -> RowList object msg
withActionBar label icon onIconClick (SelectList prop opt) =
    SelectList prop { opt | actionBar = Just ( label, icon, onIconClick ) }


withOptions : List object -> RowList object msg -> RowList object msg
withOptions items (SelectList prop opt) =
    SelectList prop { opt | items = items }


withSearchField :
    String
    -> (String -> msg)
    -> Maybe ( String, String -> object -> Bool )
    -> RowList object msg
    -> RowList object msg
withSearchField label searchMsg filter (SelectList prop opt) =
    { opt | searchField = Just <| SearchOptions label searchMsg filter }
        |> SelectList prop


withSelected : (object -> Bool) -> RowList object msg -> RowList object msg
withSelected isSelected (SelectList prop opt) =
    SelectList prop { opt | isSelected = Just isSelected }


withWidth : Element.Length -> RowList object msg -> RowList object msg
withWidth width (SelectList prop opt) =
    SelectList prop { opt | width = width }



-- Render


renderElement : RenderConfig -> RowList object msg -> Element msg
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


actionBarView : RenderConfig -> Maybe ( String, String -> Icon, msg ) -> Element msg
actionBarView cfg actionBar =
    case actionBar of
        Just ( title, icon, onClick ) ->
            Element.row
                [ Element.width fill
                , Element.paddingEach
                    { bottom = 17
                    , left = 20
                    , right = 12
                    , top = 17
                    }
                , Background.color Palette.primary.lightest
                , Font.color Palette.primary.middle
                , ARIA.roleAttr ARIA.roleButton
                , ARIA.labelAttr title
                , Element.pointer
                , Events.onClick onClick
                ]
                [ Text.body2 title
                    |> Text.withColor (Palette.color tonePrimary brightnessMiddle)
                    |> Text.renderElement cfg
                , icon title
                    |> Icon.withSize Size.small
                    |> Icon.renderElement cfg
                    |> Element.el
                        [ Element.alignRight
                        ]
                ]

        Nothing ->
            Element.none


searchFieldView : RenderConfig -> Maybe (SearchOptions object msg) -> Element msg
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


filterOptions : Maybe (SearchOptions object msg) -> List object -> List object
filterOptions searchOpt all =
    case Maybe.andThen .currentFilter searchOpt of
        Just ( value, filter ) ->
            List.filter (\obj -> filter value obj) all

        Nothing ->
            all
