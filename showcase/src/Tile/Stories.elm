module Tile.Stories exposing (stories, update)

import Element exposing (Element, fill, maximum)
import Model exposing (Model)
import Msg exposing (Msg)
import PluginOptions exposing (defaultWithMenu)
import Return exposing (Return)
import Tile.Model as TileModel exposing (Options)
import Tile.Msg as TileMsg
import UI.Effects as Effect
import UI.Icon as Icon
import UI.RenderConfig exposing (RenderConfig)
import UI.Tile as Tile
import UIExplorer exposing (storiesOf)
import Utils
    exposing
        ( ExplorerStory
        , ExplorerUI
        , goToDocsCallToAction
        , iconsSvgSprite
        , prettifyElmCode
        , story
        , storyWithModel
        )


update : TileMsg.Msg -> TileModel.Model -> Return Msg.Msg TileModel.Model
update msg model =
    case msg of
        TileMsg.Select newValue ->
            ( { model | selected = Just newValue }
            , Cmd.none
            )


stories : RenderConfig -> ExplorerUI
stories renderConfig =
    storiesOf
        "Tile"
        [ tileGroupHorizontal renderConfig
        ]


tileGroupHorizontal : RenderConfig -> ExplorerStory
tileGroupHorizontal renderConfig =
    storyWithModel
        ( "Horizontal"
        , view renderConfig
        , { defaultWithMenu
            | code = codeForHorizontalTileGroup
            , note = goToDocsCallToAction "Tile"
          }
        )


label : String
label =
    "Pick one classic rock band"


view : RenderConfig -> Model -> Element Msg
view renderConfig { tileStories } =
    Element.column
        [ Element.spacing 8, Element.width (fill |> maximum 386) ]
        [ iconsSvgSprite
        , Tile.group (TileMsg.Select >> Msg.TileStoriesMsg)
            |> Tile.withTiles
                [ Tile.selectionTile TileModel.Car <| Icon.car "Car"
                , Tile.selectionTile TileModel.Van <| Icon.van "Van"
                , Tile.selectionTile TileModel.Truck <| Icon.truck "Truck"
                , Tile.selectionTile TileModel.Motorbike <| Icon.bike "Motorbike"
                , Tile.selectionTile TileModel.Bicycle <| Icon.bicycle "Bicycle"
                , Tile.selectionTile TileModel.ECar <| Icon.eCar "E-Car"
                , Tile.selectionTile TileModel.EVan <| Icon.eVan "E-Van"
                , Tile.selectionTile TileModel.EBike <| Icon.eBike "E-Bike"
                ]
            |> Tile.withSelected tileStories.selected
            |> Tile.renderElement renderConfig
        ]


codeForHorizontalTileGroup : String
codeForHorizontalTileGroup =
    prettifyElmCode """
Tile.group
    { label = "Pick one classic rock band"
    , onSelectMsg = Msg.TileSet
    , idPrefix = "band-tile"
    }
    |> Tile.withSelected model.selected
    |> Tile.withDirection model.direction
    |> Tile.withButtons
        [ Tile.selectionTile Model.Queen <| Icon.warning "Queen"
        , Tile.selectionTile Model.Beatles <| Icon.warning "Beatles"
        , Tile.selectionTile Model.ACDC <| Icon.warning "AC/DC"
        , Tile.selectionTile Model.LedZeppelin <| Icon.warning "Led Zeppelin"
        , Tile.selectionTile Model.PinkFloyd <| Icon.warning "Pink Floyd"
        ]
    |> Tile.renderElement renderConfig
"""
