module UI.Internal.Tables.ListView exposing
    ( renderElement
    , toggleableList
    , withItems
    , withSelected
    )

import Element exposing (Attribute, Element, fill, px)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Keyed as Keyed
import UI.Icon as Icon
import UI.Internal.Basics exposing (ifThenElse, maybeAnd, prependMaybe)
import UI.Internal.Colors as Colors
import UI.Internal.Utils.Element as Element
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text exposing (ellipsize)
import UI.Utils.Element exposing (zeroPadding)


type alias Options object msg =
    { items : List object
    , select : Maybe (object -> msg)
    , isSelected : Maybe (object -> Bool)
    , width : Element.Length
    , containerId : Maybe String
    }


type alias Properties object msg =
    { toKey : object -> String
    , renderItem : RenderConfig -> Bool -> object -> Element msg
    }


{-| The `ListView object msg` type is used for describing the component for later rendering.
-}
type ListView object msg
    = SelectList (Properties object msg) (Options object msg)



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
        [ opt.items
            |> List.map
                (\obj ->
                    itemView cfg
                        prop
                        opt
                        (Just Palette.blue700)
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
    , select = Nothing
    , isSelected = Nothing
    , width = Element.fill
    , containerId = Nothing
    }



-- Filter Internals


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
