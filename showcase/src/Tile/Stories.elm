module Tile.Stories exposing (stories, update)

import Element exposing (Element, fill, maximum)
import Model exposing (Model)
import Msg exposing (Msg)
import PluginOptions exposing (defaultWithMenu)
import Return exposing (Return)
import Tile.Model as TileModel
import Tile.Msg as TileMsg
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
                [ Tile.tile TileModel.Car <| Icon.car "Car"
                , Tile.tile TileModel.Van <| Icon.van "Van"
                , Tile.tile TileModel.Truck <| Icon.truck "Truck"
                , Tile.tile TileModel.Motorbike <| Icon.bike "Motorbike"
                , Tile.tile TileModel.Bicycle <| Icon.bicycle "Bicycle"
                , Tile.tile TileModel.ECar <| Icon.eCar "E-Car"
                , Tile.tile TileModel.EVan <| Icon.eVan "E-Van"
                , Tile.tile TileModel.EBike <| Icon.eBike "E-Bike"
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
        [ Tile.tile TileModel.Car <| Icon.car "Car"
        , Tile.tile TileModel.Van <| Icon.van "Van"
        , Tile.tile TileModel.Truck <| Icon.truck "Truck"
        , Tile.tile TileModel.Motorbike <| Icon.bike "Motorbike"
        , Tile.tile TileModel.Bicycle <| Icon.bicycle "Bicycle"
        , Tile.tile TileModel.ECar <| Icon.eCar "E-Car"
        , Tile.tile TileModel.EVan <| Icon.eVan "E-Van"
        , Tile.tile TileModel.EBike <| Icon.eBike "E-Bike"
        ]
    |> Tile.renderElement renderConfig
"""
