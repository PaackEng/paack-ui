module UI.Tile exposing
    ( SelectionTiles, SelectionTile
    , tile, group
    , withTiles, withSelected, withMultipleSelected, withExplicitStateMsg
    , renderElement
    )

{-| Use this when you want [Radios](UI-Radio) with icons.

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


# Types

@docs SelectionTiles, SelectionTile


# Constructors

@docs tile, group


# Group management

@docs withTiles, withSelected, withMultipleSelected, withExplicitStateMsg


# Rendering

@docs renderElement

-}

import Element exposing (Element, fill, px)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import UI.Icon as Icon exposing (Icon)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element
import UI.Utils.Focus as Focus


{-| The `SelectionTiles option msg` type is used for describing the component for later rendering.
-}
type SelectionTiles option msg
    = SelectionTiles (Properties option msg) (Options option)


{-| The `SelectionTile option` describes an individual selection tile.
-}
type SelectionTile option
    = SelectionTile option Icon


type alias Properties option msg =
    { label : String
    , onSelectMsg : option -> Bool -> msg
    }


type alias Options option =
    { selected : List option
    , tiles : List (SelectionTile option)
    }


{-| A selection tile and an element of a selection tiles group.

    Tile.tile Model.OrangeJuice <| Icon.orangeJuice "Orange Juice"

-}
tile : option -> Icon -> SelectionTile option
tile option icon =
    SelectionTile option icon


{-| Starts an empty selection tile group.
The first argument is the message triggered when there is a selection.
The second one is the label used for accessibility (ARIA).

    someTileGroup =
        Tile.group Msg.CardPicking "Pick a card"

-}
group : (option -> msg) -> String -> SelectionTiles option msg
group onSelectMsg label =
    SelectionTiles
        { label = label
        , onSelectMsg = \k _ -> onSelectMsg k
        }
        { selected = []
        , tiles = []
        }


{-| Replaces a group's list of selection tiles.

    Tile.withTiles
        [ Tile.tile Model.OrangeJuice <| Icon.orangeJuice "Orange Juice"
        , Tile.tile Model.Lemonade <| Icon.lemonade "Lemonade"
        , Tile.tile Model.SodaSoftDrink <| Icon.sodaSoftDrink "Soda"
        ]
        someTileGroup

-}
withTiles : List (SelectionTile option) -> SelectionTiles option msg -> SelectionTiles option msg
withTiles tiles (SelectionTiles prop opt) =
    SelectionTiles prop
        { opt | tiles = tiles }


{-| Define one element as selected.

    Tile.withSelected (Just Model.DoubleCheddar)

-}
withSelected : Maybe option -> SelectionTiles option msg -> SelectionTiles option msg
withSelected newSelected (SelectionTiles prop opt) =
    SelectionTiles prop
        { opt
            | selected =
                newSelected
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
        }


{-| Define multiple elements as selected.

    Tile.withMultipleSelected [ Model.DoubleCheddar, Model.XSalad ]

-}
withMultipleSelected : List option -> SelectionTiles option msg -> SelectionTiles option msg
withMultipleSelected newSelected (SelectionTiles prop opt) =
    SelectionTiles prop
        { opt | selected = newSelected }


{-| Receive in the message the correct new state of the tile.

    Tile.group
        (\which newBooleanState -> SetSomeTile which newBooleanState)
        "SomeGroup"
        |> Tile.withExplicitStateMsg

-}
withExplicitStateMsg : SelectionTiles option (Bool -> msg2) -> SelectionTiles option msg2
withExplicitStateMsg (SelectionTiles prop opt) =
    SelectionTiles { label = prop.label, onSelectMsg = \k -> prop.onSelectMsg k True } opt


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> SelectionTiles option msg -> Element msg
renderElement renderConfig (SelectionTiles prop opt) =
    let
        ariaAttrs =
            ARIA.roleRadioGroup prop.label
                |> ARIA.toElementAttributes
    in
    opt.tiles
        |> List.map (tileRender renderConfig prop.onSelectMsg opt.selected)
        |> Element.wrappedRow (Element.spacing 14 :: ariaAttrs)


tileRender : RenderConfig -> (option -> Bool -> msg) -> List option -> SelectionTile option -> Element msg
tileRender renderConfig onSelectMsg selected (SelectionTile option icon) =
    let
        isSelected =
            List.member option selected

        selectThisMsg =
            onSelectMsg option <| not isSelected

        ( backgroundColor, borderColor ) =
            if isSelected then
                ( { normal = Palette.blue100, hovered = Palette.blue200, focused = Palette.blue200 }
                , { normal = Palette.blue700, hovered = Palette.blue700, focused = Palette.blue600 }
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

        ariaAttrs =
            ARIA.roleRadio isSelected
                |> ARIA.toElementAttributes

        attributes =
            Focus.focus isSelected
                |> Focus.withTabIndex 0
                |> Focus.toElementAttributes
                |> (++) baseAttrs
                |> (++) ariaAttrs
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
