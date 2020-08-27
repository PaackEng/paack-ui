module Icons exposing (stories)

import Element exposing (Element)
import Element.Font as Font
import Html exposing (Html)
import PluginOptions exposing (defaultWithoutMenu)
import UI.Icon as Icon exposing (Icon)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UIExplorer exposing (storiesOf)
import Utils exposing (ExplorerUI, goToDocsCallToAction, iconsSvgSprite, prettifyElmCode)


stories : RenderConfig -> ExplorerUI
stories cfg =
    storiesOf
        "Icons"
        [ ( "IconsExample"
          , \_ -> iconsView cfg
          , { defaultWithoutMenu
                | code = code
                , note = note
            }
          )
        ]


code : String
code =
    prettifyElmCode """
Icon.seeMore label
    |> Icon.withColor (Palette.color Palette.tonePrimary Palette.brightnessMiddle)
    |> Icon.renderElement cfg
"""


note : String
note =
    """
We name icons by their functionality and not their shapes.

"""
        ++ goToDocsCallToAction "Icon"


icons : List ( String -> Icon, String )
icons =
    [ ( Icon.add, "Add" )
    , ( Icon.close, "close" )
    , ( Icon.edit, "edit" )
    , ( Icon.eventLog, "eventLog" )
    , ( Icon.logout, "logout" )
    , ( Icon.notifications, "notifications" )
    , ( Icon.paackSpaces, "paackSpaces" )
    , ( Icon.packages, "packages" )
    , ( Icon.print, "print" )
    , ( Icon.sandwichMenu, "sandwichMenu" )
    , ( Icon.search, "search" )
    , ( Icon.toggle, "toggle" )
    , ( Icon.toggleUp, "toggleUp" )
    , ( Icon.toggleDown, "toggleDown" )
    , ( Icon.previousContent, "previousContent" )
    , ( Icon.nextContent, "nextContent" )
    , ( Icon.seeMore, "seeMore" )
    , ( Icon.warning, "warning" )
    , ( Icon.filter, "filter" )
    ]


iconView : RenderConfig -> Palette.Color -> ( String -> Icon, String ) -> Element msg
iconView cfg color ( iconFn, label ) =
    Element.column
        [ Element.spacing 10
        , Element.padding 10
        , Element.width (Element.px 100)
        , Element.height (Element.px 100)
        ]
        [ iconFn label
            |> Icon.withColor color
            |> Icon.renderElement cfg
            |> Element.el [ Element.centerX ]
        , Element.el [ Font.size 14, Element.centerX ] <| Element.text label
        ]


iconsGroup : RenderConfig -> Palette.Color -> Element msg
iconsGroup cfg color =
    icons
        |> List.map (iconView cfg color)
        |> (::) iconsSvgSprite
        |> Element.wrappedRow
            [ Element.spacing 10
            ]


iconsView : RenderConfig -> Html msg
iconsView cfg =
    Element.layout [] <|
        Element.column
            []
            [ iconsGroup cfg
                (Palette.color Palette.toneGray Palette.brightnessMiddle)
            ]
