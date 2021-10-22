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
    [ ( Icon.add, "add" )
    , ( Icon.arrowUp, "arrowUp" )
    , ( Icon.assignPerson, "assignPerson" )
    , ( Icon.bicycle, "bicycle" )
    , ( Icon.bike, "bike" )
    , ( Icon.boxes, "boxes" )
    , ( Icon.car, "car" )
    , ( Icon.check, "check" )
    , ( Icon.clockIssue, "clockIssue" )
    , ( Icon.clockLocked, "clockLocked" )
    , ( Icon.coins, "coins" )
    , ( Icon.success, "success" )
    , ( Icon.close, "close" )
    , ( Icon.collapse, "collapse" )
    , ( Icon.configure, "configure" )
    , ( Icon.delete, "delete" )
    , ( Icon.done, "done" )
    , ( Icon.download, "download" )
    , ( Icon.eBike, "eBike" )
    , ( Icon.eCar, "eCar" )
    , ( Icon.eVan, "eVan" )
    , ( Icon.edit, "edit" )
    , ( Icon.eventLog, "eventLog" )
    , ( Icon.expand, "expand" )
    , ( Icon.filter, "filter" )
    , ( Icon.fix, "fix" )
    , ( Icon.fixIssues, "fixIssues" )
    , ( Icon.fixing, "fixing" )
    , ( Icon.groups, "groups" )
    , ( Icon.insert, "insert" )
    , ( Icon.legacyApis, "legacyApis" )
    , ( Icon.legacyHistorical, "Historical" )
    , ( Icon.legacyLabelPrinter, "legacyLabelPrinter" )
    , ( Icon.legacyReport, "legacyReport" )
    , ( Icon.legacyRetailerDashboard, "legacyRetailerDashboard" )
    , ( Icon.legacySignOut, "legacySignOut" )
    , ( Icon.location, "location" )
    , ( Icon.loader, "loader" )
    , ( Icon.lock, "lock" )
    , ( Icon.logout, "logout" )
    , ( Icon.messageLock, "messageLock" )
    , ( Icon.messageOTP, "messageOTP" )
    , ( Icon.microphoneMute, "microphoneMute" )
    , ( Icon.moreActions, "moreActions" )
    , ( Icon.move, "move" )
    , ( Icon.nextContent, "nextContent" )
    , ( Icon.notifications, "notifications" )
    , ( Icon.otp, "OTP" )
    , ( Icon.pause, "pause" )
    , ( Icon.paackSpaces, "paackSpaces" )
    , ( Icon.packages, "packages" )
    , ( Icon.person, "person" )
    , ( Icon.persons, "persons" )
    , ( Icon.phone, "phone" )
    , ( Icon.previousContent, "previousContent" )
    , ( Icon.print, "print" )
    , ( Icon.reload, "reload" )
    , ( Icon.remove, "remove" )
    , ( Icon.sandwichMenu, "sandwichMenu" )
    , ( Icon.search, "search" )
    , ( Icon.searchSpace, "searchSpace" )
    , ( Icon.seeMore, "seeMore" )
    , ( Icon.sort, "sort" )
    , ( Icon.sortDecreasing, "sortDecreasing" )
    , ( Icon.sortIncreasing, "sortIncreasing" )
    , ( Icon.toggle, "toggle" )
    , ( Icon.toggleDown, "toggleDown" )
    , ( Icon.toggleUp, "toggleUp" )
    , ( Icon.truck, "truck" )
    , ( Icon.unassignPerson, "unassignPerson" )
    , ( Icon.van, "van" )
    , ( Icon.wait, "wait" )
    , ( Icon.warning, "warning" )
    , ( Icon.webhook, "webhook" )
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
                Palette.gray700
            ]
