module UI.V2.SummaryListItem exposing
    ( SummaryListItem, summaryListItem
    , withSelected, withBadge, withCheckbox
    , renderElement
    )

{-| `SummaryListItem` represents a single item in a list that is usually selectable or searchable.
This component is meant to be used with [`ListView`](UI-ListView).

    import UI.V2.SummaryListItem as Summary

    listItemView : AppConfig -> Bool -> Item -> Element Msg
    listItemView appConfig isSelected item =
        Summary.summaryListItem item.name item.subtitle
            |> Summary.withSelected isSelected
            |> Summary.withBadge (Badge.grayLight "badge")
            |> Summary.renderElement appConfig.renderConfig

    listView : AppConfig -> Model -> ListView Item Msg
    listView appConfig model =
        listItemView appConfig
            |> ListView.selectList Msg.Select Item.id
            |> ListView.withSearchField (searchField appConfig model)
            |> ListView.withActionBar (actionBar appConfig)
            |> ListView.withDomId "item-list"


# Building

@docs SummaryListItem, summaryListItem


# Options

@docs withSelected, withBadge, withCheckbox


# Rendering

@docs renderElement

-}

import Element exposing (Element, fill)
import Element.Keyed as Keyed
import UI.Badge as Badge exposing (Badge)
import UI.Checkbox as Checkbox exposing (Checkbox)
import UI.Internal.Basics exposing (prependMaybe)
import UI.Palette as Palette
    exposing
        ( brightnessLight
        , brightnessLighter
        , brightnessMiddle
        , toneGray
        , tonePrimary
        )
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text exposing (ellipsize)



-- Building


{-| The `SummaryListItem msg` type is used for describing the component for later
rendering.
-}
type SummaryListItem msg
    = SummaryListItem Properties (Options msg)


type alias Properties =
    { title : String
    , caption : String
    }


type alias Options msg =
    { selected : Bool
    , badge : Maybe Badge
    , checkbox : Maybe (Checkbox msg)
    }


{-| Constructs a summary by receiving its title and caption.

    Summary.summaryListItem "Item title" "Item caption"

-}
summaryListItem : String -> String -> SummaryListItem msg
summaryListItem title caption =
    SummaryListItem (Properties title caption) (Options False Nothing Nothing)


{-| Indicates whether the summary is selected or not.

    Summary.withSelected True
        someListView

-}
withSelected : Bool -> SummaryListItem msg -> SummaryListItem msg
withSelected selected (SummaryListItem prop opt) =
    SummaryListItem prop { opt | selected = selected }


{-| Adds a badge to the right side of the summary.

    Summary.withBadge someBadge
        someListView

-}
withBadge : Badge -> SummaryListItem msg -> SummaryListItem msg
withBadge badge (SummaryListItem prop opt) =
    SummaryListItem prop { opt | badge = Just badge }


{-| Adds a checkbox to the left side of the summary.

    Summary.withCheckbox someCheckbox
        someListView

-}
withCheckbox : Checkbox msg -> SummaryListItem msg -> SummaryListItem msg
withCheckbox checkbox (SummaryListItem prop opt) =
    SummaryListItem prop { opt | checkbox = Just checkbox }


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> SummaryListItem msg -> Element msg
renderElement renderConfig (SummaryListItem { title, caption } opt) =
    let
        colors =
            listItemColors opt

        label =
            ( "label", listItemLabel renderConfig colors title caption )

        maybeBadge =
            case opt.badge of
                Just badge ->
                    Just ( "badge", listItemBadge renderConfig <| colors.badge badge )

                Nothing ->
                    Nothing

        maybeCheckbox =
            case opt.checkbox of
                Just checkbox ->
                    Just ( "checkbox", listItemCheckbox renderConfig checkbox )

                Nothing ->
                    Nothing
    in
    Keyed.row
        [ Element.width fill
        , Element.paddingEach { top = 11, bottom = 11, left = 15, right = 12 }
        ]
        (maybeBadge
            |> Maybe.map List.singleton
            |> Maybe.withDefault []
            |> (::) label
            |> prependMaybe maybeCheckbox
        )



-- Internal


type alias ColorsHelper =
    { badge : Badge -> Badge
    , title : Palette.Color
    , caption : Palette.Color
    }


listItemLabel : RenderConfig -> ColorsHelper -> String -> String -> Element msg
listItemLabel renderConfig colors title caption =
    Keyed.column
        [ Element.width fill, Element.clipX, Element.spacing 4 ]
        [ ( "title", listItemTitle renderConfig colors.title title )
        , ( "caption", listItemCaption renderConfig colors.caption caption )
        ]


listItemBadge : RenderConfig -> Badge -> Element msg
listItemBadge renderConfig badge =
    badge
        |> Badge.renderElement renderConfig
        |> Element.el [ Element.alignTop ]


listItemTitle : RenderConfig -> Palette.Color -> String -> Element msg
listItemTitle renderConfig color title =
    Text.body1 title
        |> Text.withOverflow ellipsize
        |> Text.withColor color
        |> Text.renderElement renderConfig
        |> Element.el [ Element.width fill ]


listItemCaption : RenderConfig -> Palette.Color -> String -> Element msg
listItemCaption renderConfig color caption =
    Text.caption caption
        |> Text.withOverflow ellipsize
        |> Text.withColor color
        |> Text.renderElement renderConfig


listItemColors : Options msg -> ColorsHelper
listItemColors { selected, checkbox } =
    let
        defaultColors =
            { badge = identity
            , title =
                Palette.color tonePrimary brightnessLighter
                    |> Palette.setContrasting True
            , caption = Palette.color toneGray brightnessMiddle
            }
    in
    if selected then
        case checkbox of
            Just _ ->
                defaultColors

            Nothing ->
                { badge = Badge.withTone Badge.primaryDark
                , title =
                    Palette.color tonePrimary brightnessMiddle
                        |> Palette.setContrasting True
                , caption = Palette.color tonePrimary brightnessLight
                }

    else
        defaultColors


listItemCheckbox : RenderConfig -> Checkbox msg -> Element msg
listItemCheckbox renderConfig checkbox =
    checkbox
        |> Checkbox.renderElement renderConfig
        |> Element.el [ Element.alignTop ]
