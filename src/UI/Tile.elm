module UI.Tile exposing
    ( SelectionTiles, SelectionTile
    , selectionTile, group
    , withSelected
    , renderElement
    )

{-|

@docs SelectionTiles, SelectionTile

@docs selectionTile, group

@docs withSelected

@docs renderElement

-}

import Element exposing (Attribute, Element)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import UI.Icon as Icon exposing (Icon)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text


type SelectionTiles value msg
    = SelectionTiles (Properties value msg) (Options value)


type SelectionTile value
    = SelectionTile value Icon


type alias Properties value msg =
    { tiles : List (SelectionTile value)
    , onSelectMsg : value -> msg
    }


type alias Options value =
    { selected : Maybe value
    }


selectionTile : value -> Icon -> SelectionTile value
selectionTile value icon =
    SelectionTile value icon


group : (value -> msg) -> List (SelectionTile value) -> SelectionTiles value msg
group onSelectMsg tiles =
    SelectionTiles
        { tiles = tiles
        , onSelectMsg = onSelectMsg
        }
        { selected = Nothing }


withSelected : value -> SelectionTiles value msg -> SelectionTiles value msg
withSelected newSelected (SelectionTiles prop opt) =
    SelectionTiles prop { opt | selected = Just newSelected }


renderElement : RenderConfig -> SelectionTiles value msg -> Element msg
renderElement renderConfig (SelectionTiles prop opt) =
    prop.tiles
        |> List.map (tileRender renderConfig prop.onSelectMsg opt.selected)
        |> Element.wrappedRow [ Element.spacing 14 ]


tileRender : RenderConfig -> (value -> msg) -> Maybe value -> SelectionTile value -> Element msg
tileRender renderConfig onSelectMsg selected (SelectionTile value icon) =
    Element.column
        [ Border.width 3
        , Border.rounded 6
        , Palette.toBackgroundColor Palette.gray100
        , Palette.toBorderColor Palette.gray100
        , Events.onClick (onSelectMsg value)
        , Element.paddingXY 12 20
        , Font.center
        ]
        [ icon
            |> Icon.withColor Palette.blue700
            |> Icon.withCustomSize 40
            |> Icon.renderElement renderConfig
        , Icon.getHint icon
            |> Text.subtitle2
            |> Text.withColor Palette.blue700
            |> Text.renderElement renderConfig
        ]
