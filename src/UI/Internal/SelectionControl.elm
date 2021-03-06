module UI.Internal.SelectionControl exposing (..)

import Element exposing (Attribute, px, shrink)
import Element.Background as Background
import Element.Border as Border
import UI.Internal.Colors as Colors


type SelectionControlSize
    = SizeSM
    | SizeMD


type alias Sizes =
    { icon : Int, padding : Int, border : Int }


iconColor : Bool -> Element.Color
iconColor active =
    if active then
        Colors.navyBlue700

    else
        Colors.gray600


sizes : SelectionControlSize -> Sizes
sizes size =
    case size of
        SizeSM ->
            Sizes 20 8 2

        SizeMD ->
            Sizes 28 10 3


buttonAttributes : SelectionControlSize -> List (Element.Attribute msg)
buttonAttributes size =
    [ Element.spacing 10
    , Element.width shrink
    , Element.padding <| .padding <| sizes size
    , Element.pointer
    , Border.rounded 6
    , Element.mouseOver [ Background.color <| Colors.gray200 ]
    , Element.focused
        [ Border.innerShadow
            { offset = ( 0, 0 )
            , size = 2
            , blur = 0
            , color = Colors.navyBlue600
            }
        ]
    ]


iconAttributes : SelectionControlSize -> Bool -> List (Attribute msg)
iconAttributes size active =
    let
        { icon, border } =
            sizes size
    in
    [ Element.width (px icon)
    , Element.height (px icon)
    , Border.color <| iconColor active
    , Border.width border
    , Element.alignTop
    ]
