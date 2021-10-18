module UI.Tile exposing
    ( SelectionTiles, SelectionTile
    , selectionTile, group, withTiles
    , withSelected
    , renderElement
    )

{-|

@docs SelectionTiles, SelectionTile

@docs selectionTile, group, withTiles

@docs withSelected

@docs renderElement

-}

import Element exposing (Attribute, Element, fill, px)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import UI.Icon as Icon exposing (Icon)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.Element as Element
import UI.Utils.Focus as Focus


type SelectionTiles value msg
    = SelectionTiles (Properties value msg) (Options value)


type SelectionTile value
    = SelectionTile value Icon


type alias Properties value msg =
    { onSelectMsg : value -> msg
    }


type alias Options value =
    { selected : Maybe value
    , tiles : List (SelectionTile value)
    }


selectionTile : value -> Icon -> SelectionTile value
selectionTile value icon =
    SelectionTile value icon


group : (value -> msg) -> SelectionTiles value msg
group onSelectMsg =
    SelectionTiles
        { onSelectMsg = onSelectMsg
        }
        { selected = Nothing
        , tiles = []
        }


withTiles : List (SelectionTile value) -> SelectionTiles value msg -> SelectionTiles value msg
withTiles tiles (SelectionTiles prop opt) =
    SelectionTiles prop
        { opt | tiles = tiles }


withSelected : Maybe value -> SelectionTiles value msg -> SelectionTiles value msg
withSelected newSelected (SelectionTiles prop opt) =
    SelectionTiles prop { opt | selected = newSelected }


renderElement : RenderConfig -> SelectionTiles value msg -> Element msg
renderElement renderConfig (SelectionTiles prop opt) =
    opt.tiles
        |> List.map (tileRender renderConfig prop.onSelectMsg opt.selected)
        |> Element.wrappedRow [ Element.spacing 14 ]


tileRender : RenderConfig -> (value -> msg) -> Maybe value -> SelectionTile value -> Element msg
tileRender renderConfig onSelectMsg selected (SelectionTile value icon) =
    let
        isSelected =
            selected == Just value

        selectThisMsg =
            onSelectMsg value

        ( backgroundColor, borderColor ) =
            if isSelected then
                ( { normal = Palette.blue100, hovered = Palette.blue200, focused = Palette.blue200 }
                , { normal = Palette.blue700, hovered = Palette.blue700, focused = Palette.blue700 }
                )

            else
                ( { normal = Palette.gray100, hovered = Palette.gray200, focused = Palette.genericWhite }
                , { normal = Palette.gray100, hovered = Palette.gray200, focused = Palette.blue600 }
                )

        baseAttrs =
            [ Border.width 3
            , Border.rounded 6
            , Palette.toBackgroundColor backgroundColor.normal
            , Palette.toBorderColor borderColor.normal
            , Events.onClick selectThisMsg
            , Element.onEnterPressed selectThisMsg
            , Element.pointer
            , Font.center
            , Element.width (px 86)
            , Element.height (px 86)
            , Element.focused
                [ Palette.toBackgroundColor backgroundColor.focused
                , Palette.toBorderColor borderColor.focused
                ]
            , Element.mouseOver
                [ Palette.toBackgroundColor backgroundColor.hovered
                , Palette.toBorderColor borderColor.hovered
                ]
            ]

        attributes =
            Focus.focus isSelected
                |> Focus.withTabIndex 0
                |> Focus.toElementAttributes
                |> (++) baseAttrs
    in
    Element.column attributes
        [ icon
            |> Icon.withColor Palette.blue700
            |> Icon.withCustomSize 40
            |> Icon.renderElement renderConfig
            |> Element.el [ Element.centerY, Element.centerX ]
        , Icon.getHint icon
            |> Text.subtitle2
            |> Text.withColor Palette.blue700
            |> Text.withOverflow Text.ellipsizeWithTooltip
            |> Text.renderElement renderConfig
            |> Element.el
                [ Element.width fill
                , Element.clipX
                , Element.height (px 18)
                , Element.centerY
                ]
        ]
