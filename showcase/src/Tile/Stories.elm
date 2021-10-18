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


view : RenderConfig -> Model -> Element Msg
view renderConfig { tileStories } =
    Element.column
        [ Element.spacing 8, Element.width (fill |> maximum 386) ]
        [ iconsSvgSprite
        , Tile.group (TileMsg.Select >> Msg.TileStoriesMsg)
            "Pick a delivery vehicle"
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
Tile.group Msg.SetTile
    "Pick a delivery vehicle"
    |> Tile.withSelected model.selected
    |> Tile.withButtons
        [ Tile.selectionTile TileModel.Car <| Icon.car "Car"
        , Tile.selectionTile TileModel.Van <| Icon.van "Van"
        , Tile.selectionTile TileModel.Truck <| Icon.truck "Truck"
        , Tile.selectionTile TileModel.Motorbike <| Icon.bike "Motorbike"
        , Tile.selectionTile TileModel.Bicycle <| Icon.bicycle "Bicycle"
        , Tile.selectionTile TileModel.ECar <| Icon.eCar "E-Car"
        , Tile.selectionTile TileModel.EVan <| Icon.eVan "E-Van"
        , Tile.selectionTile TileModel.EBike <| Icon.eBike "E-Bike"
        ]
    |> Tile.renderElement renderConfig
"""
