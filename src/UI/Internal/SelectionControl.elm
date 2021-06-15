module UI.Internal.SelectionControl exposing (..)

import Element exposing (Attribute, Element, fill, px, shrink)
import Element.Background as Background
import Element.Border as Border
import Html.Attributes as HtmlAttrs
import UI.Internal.Colors as Colors
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text


type SelectionControlSize
    = SizeSM
    | SizeMD


type alias Sizes =
    { icon : Int, padding : Int, border : Int }


iconColor : Bool -> Element.Color
iconColor active =
    if active then
        Colors.primary.middle

    else
        Colors.gray.light1


sizes : SelectionControlSize -> Sizes
sizes size =
    case size of
        SizeSM ->
            Sizes 20 8 2

        SizeMD ->
            Sizes 28 10 3


buttonAttributes : SelectionControlSize -> Bool -> List (Element.Attribute msg)
buttonAttributes size active =
    [ Element.spacing 10
    , Element.width shrink
    , Element.padding <| .padding <| sizes size
    , Element.pointer
    , Border.rounded 6
    , Element.mouseOver [ Background.color <| Colors.gray.light3 ]
    , Element.htmlAttribute <| HtmlAttrs.tabindex 0
    , Element.focused
        [ Border.innerShadow
            { offset = ( 0, 0 )
            , size = 2
            , blur = 0
            , color = Colors.primary.middle
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
    ]


label : RenderConfig -> String -> Element msg
label renderConfig =
    Text.body1 >> Text.renderElement renderConfig
