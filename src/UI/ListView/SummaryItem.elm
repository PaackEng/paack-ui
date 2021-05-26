module UI.ListView.SummaryItem exposing
    ( SummaryItem, summaryItem
    , withSelected, withBadge, withCheckbox, withIcon, withBulletIcon, withBackgroundTone
    , renderElement
    )

{-| `SummaryItem` represents a single item in a list that is usually selectable or searchable.
This component is meant to be used with [`ListView`](UI-ListView).

    import UI.ListView.SummaryItem as Summary

    listItemView : AppConfig -> Bool -> Item -> Element Msg
    listItemView appConfig isSelected item =
        Summary.summaryItem item.name item.subtitle
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

@docs SummaryItem, summaryItem


# Options

@docs withSelected, withBadge, withCheckbox, withIcon, withBulletIcon, withBackgroundTone


# Rendering

@docs renderElement

-}

import Element exposing (Element, fill, px)
import Element.Background as Background
import Element.Border as Border
import Element.Keyed as Keyed
import UI.Badge as Badge exposing (Badge)
import UI.Checkbox as Checkbox exposing (Checkbox)
import UI.Icon as Icon exposing (Icon)
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
import UI.Size as Size
import UI.Text as Text exposing (ellipsize)



-- Building


{-| The `SummaryItem msg` type is used for describing the component for later
rendering.
-}
type SummaryItem msg
    = SummaryItem Properties (Options msg)


type alias Properties =
    { title : String
    , caption : String
    }


type alias Options msg =
    { selected : Bool
    , badge : Maybe Badge
    , checkbox : Maybe (Checkbox msg)
    , bullet : Maybe Icon
    , icon : Maybe Icon
    , backgroundTone : Maybe Palette.Tone
    }


{-| Constructs a summary by receiving its title and caption.

    Summary.summaryItem "Item title" "Item caption"

-}
summaryItem : String -> String -> SummaryItem msg
summaryItem title caption =
    SummaryItem
        (Properties title caption)
        (Options False Nothing Nothing Nothing Nothing Nothing)


{-| Indicates whether the summary is selected or not.

    Summary.withSelected True
        someListView

-}
withSelected : Bool -> SummaryItem msg -> SummaryItem msg
withSelected selected (SummaryItem prop opt) =
    SummaryItem prop { opt | selected = selected }


{-| Adds an icon to the right side of the summary.

    Summary.withIcon (Icon.fix "Problem Solving")
        someListView

-}
withIcon : Icon -> SummaryItem msg -> SummaryItem msg
withIcon icon (SummaryItem prop opt) =
    SummaryItem prop { opt | icon = Just icon }


{-| Adds an icon to the left side of the summary.

    Summary.withBulletIcon (Icon.fixIssues "Problem Solving")
        someListView

-}
withBulletIcon : Icon -> SummaryItem msg -> SummaryItem msg
withBulletIcon icon (SummaryItem prop opt) =
    SummaryItem prop { opt | bullet = Just icon }


{-| Applies a tone to summary's background.

    Summary.withBackgroundTone Palette.toneWarning
        someListView

-}
withBackgroundTone : Palette.Tone -> SummaryItem msg -> SummaryItem msg
withBackgroundTone tone (SummaryItem prop opt) =
    SummaryItem prop { opt | backgroundTone = Just tone }


{-| Adds a badge to the right side of the summary.

    Summary.withBadge someBadge
        someListView

-}
withBadge : Badge -> SummaryItem msg -> SummaryItem msg
withBadge badge (SummaryItem prop opt) =
    SummaryItem prop { opt | badge = Just badge }


{-| Adds a checkbox to the left side of the summary.

    Summary.withCheckbox someCheckbox
        someListView

-}
withCheckbox : Checkbox msg -> SummaryItem msg -> SummaryItem msg
withCheckbox checkbox (SummaryItem prop opt) =
    SummaryItem prop { opt | checkbox = Just checkbox }


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> SummaryItem msg -> Element msg
renderElement renderConfig (SummaryItem { title, caption } opt) =
    let
        colors =
            listItemColors opt
    in
    opt.badge
        |> Maybe.map
            (colors.badge >> listItemBadge renderConfig >> List.singleton)
        |> Maybe.withDefault []
        |> prependMaybe
            (Maybe.map (listItemIcon renderConfig) opt.icon)
        |> (::)
            (listItemLabel renderConfig colors title caption)
        |> prependMaybe
            (Maybe.map (listItemBullet renderConfig colors.tone opt.selected) opt.bullet)
        |> prependMaybe
            (Maybe.map (listItemCheckbox renderConfig) opt.checkbox)
        |> Keyed.row
            (Element.width fill
                :: Element.paddingEach { top = 11, bottom = 11, left = 15, right = 12 }
                :: (Maybe.withDefault [] <|
                        Maybe.map (backgroundColor >> List.singleton) opt.backgroundTone
                   )
            )



-- Internal


type alias ColorsHelper =
    { badge : Badge -> Badge
    , title : Palette.Color
    , caption : Palette.Color
    , tone : Palette.Tone
    }


backgroundColor : Palette.Tone -> Element.Attr decorative msg
backgroundColor tone =
    Palette.color tone Palette.brightnessLight4
        |> Palette.toElementColor
        |> Background.color


listItemColors : Options msg -> ColorsHelper
listItemColors opt =
    let
        defaultColors =
            { badge = identity
            , title =
                Palette.color tonePrimary brightnessLighter
                    |> Palette.setContrasting True
            , caption = Palette.color toneGray brightnessMiddle
            , tone = tonePrimary
            }
    in
    if opt.selected then
        case opt.checkbox of
            Just _ ->
                defaultColors

            Nothing ->
                { badge = Badge.withTone Badge.primaryDark
                , title =
                    Palette.color tonePrimary brightnessMiddle
                        |> Palette.setContrasting True
                , caption = Palette.color tonePrimary brightnessLight
                , tone = tonePrimary
                }

    else
        { defaultColors | tone = Maybe.withDefault tonePrimary opt.backgroundTone }


listItemCheckbox : RenderConfig -> Checkbox msg -> ( String, Element msg )
listItemCheckbox renderConfig checkbox =
    checkbox
        |> Checkbox.renderElement renderConfig
        |> Element.el [ Element.alignTop ]
        |> Tuple.pair "checkbox"


listItemBullet : RenderConfig -> Palette.Tone -> Bool -> Icon -> ( String, Element msg )
listItemBullet renderConfig tone isSelected icon =
    icon
        |> Icon.withCustomSize 11
        |> Icon.withColor (Palette.color tone Palette.brightnessDarkest)
        |> Icon.renderElement renderConfig
        |> Element.el
            [ Element.centerY
            , Element.centerX
            ]
        |> Element.el
            [ Element.width (px 20)
            , Element.height (px 20)
            , Border.rounded 10
            , Palette.color tone (brightness isSelected)
                |> Palette.toElementColor
                |> Background.color
            ]
        |> Element.el
            [ Element.alignTop
            , Element.paddingEach { top = 0, right = 8, bottom = 0, left = 0 }
            ]
        |> Tuple.pair "bullet"


brightness : Bool -> Palette.Brightness
brightness isSelected =
    if isSelected then
        Palette.brightnessLighter

    else
        Palette.brightnessLight


listItemLabel :
    RenderConfig
    -> ColorsHelper
    -> String
    -> String
    -> ( String, Element msg )
listItemLabel renderConfig colors title caption =
    ( "label"
    , Keyed.column
        [ Element.width fill, Element.clipX, Element.spacing 4 ]
        [ listItemTitle renderConfig colors.title title
        , listItemCaption renderConfig colors.caption caption
        ]
    )


listItemTitle : RenderConfig -> Palette.Color -> String -> ( String, Element msg )
listItemTitle renderConfig color title =
    Text.subtitle1 title
        |> Text.withOverflow ellipsize
        |> Text.withColor color
        |> Text.renderElement renderConfig
        |> Tuple.pair "text"
        |> Keyed.el [ Element.width fill ]
        |> Tuple.pair "title"


listItemCaption : RenderConfig -> Palette.Color -> String -> ( String, Element msg )
listItemCaption renderConfig color caption =
    Text.caption caption
        |> Text.withOverflow ellipsize
        |> Text.withColor color
        |> Text.renderElement renderConfig
        |> Tuple.pair "caption"


listItemIcon : RenderConfig -> Icon -> ( String, Element msg )
listItemIcon renderConfig icon =
    icon
        |> Icon.withSize Size.extraSmall
        |> Icon.renderElement renderConfig
        |> Element.el
            [ Element.alignTop
            , Element.alignLeft
            , Element.paddingEach
                { left = 4, right = 4, top = 2, bottom = 0 }
            ]
        |> Tuple.pair "icon"


listItemBadge : RenderConfig -> Badge -> ( String, Element msg )
listItemBadge renderConfig badge =
    badge
        |> Badge.renderElement renderConfig
        |> Element.el [ Element.alignTop ]
        |> Tuple.pair "badge"
