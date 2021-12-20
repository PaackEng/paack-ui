module Icons exposing (stories)

import Element exposing (Element, fill, px)
import Element.Font as Font
import Html exposing (Html)
import PluginOptions exposing (defaultWithoutMenu)
import UI.Icon as Icon exposing (Icon)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text exposing (ellipsize)
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
    goToDocsCallToAction "Icon"


icons : List ( String -> Icon, String )
icons =
    [ ( Icon.addCircle, "addCircle" )
    , ( Icon.add, "add" )
    , ( Icon.arrowCurveLeft, "arrowCurveLeft" )
    , ( Icon.arrowDown, "arrowDown" )
    , ( Icon.arrowLeft, "arrowLeft" )
    , ( Icon.arrowRight, "arrowRight" )
    , ( Icon.arrowUp, "arrowUp" )
    , ( Icon.bell, "bell" )
    , ( Icon.bicycle, "bicycle" )
    , ( Icon.bike, "bike" )
    , ( Icon.bluetooth, "bluetooth" )
    , ( Icon.boltDisabled, "boltDisabled" )
    , ( Icon.bolt, "bolt" )
    , ( Icon.boxFilled, "boxFilled" )
    , ( Icon.boxOutlined, "boxOutlined" )
    , ( Icon.boxesFilled, "boxesFilled" )
    , ( Icon.boxesOutlined, "boxesOutlined" )
    , ( Icon.camera, "camera" )
    , ( Icon.car, "car" )
    , ( Icon.cellular, "cellular" )
    , ( Icon.checkmarkCircle, "checkmarkCircle" )
    , ( Icon.checkmarkRoundedRectangle, "checkmarkRoundedRectangle" )
    , ( Icon.checkmark, "checkmark" )
    , ( Icon.chevronDown, "chevronDown" )
    , ( Icon.chevronLeft, "chevronLeft" )
    , ( Icon.chevronRight, "chevronRight" )
    , ( Icon.chevronUp, "chevronUp" )
    , ( Icon.clockIssue, "clockIssue" )
    , ( Icon.clockLocked, "clockLocked" )
    , ( Icon.clock, "clock" )
    , ( Icon.closeCircle, "closeCircle" )
    , ( Icon.closeRoundedRectangle, "closeRoundedRectangle" )
    , ( Icon.close, "close" )
    , ( Icon.coins, "coins" )
    , ( Icon.collapse, "collapse" )
    , ( Icon.crosshair, "crosshair" )
    , ( Icon.directions, "directions" )
    , ( Icon.download, "download" )
    , ( Icon.eBike, "eBike" )
    , ( Icon.eCar, "eCar" )
    , ( Icon.eVan, "eVan" )
    , ( Icon.edit, "edit" )
    , ( Icon.ellipsis, "ellipsis" )
    , ( Icon.empty, "empty" )
    , ( Icon.expand, "expand" )
    , ( Icon.eyeHide, "eyeHide" )
    , ( Icon.eye, "eye" )
    , ( Icon.filter, "filter" )
    , ( Icon.fix, "fix" )
    , ( Icon.fixing, "fixing" )
    , ( Icon.folder, "folder" )
    , ( Icon.food, "food" )
    , ( Icon.frozen, "frozen" )
    , ( Icon.gVan, "gVan" )
    , ( Icon.grocery, "grocery" )
    , ( Icon.groups, "groups" )
    , ( Icon.hamburger, "hamburger" )
    , ( Icon.hand, "hand" )
    , ( Icon.home, "home" )
    , ( Icon.hourglass, "hourglass" )
    , ( Icon.info, "info" )
    , ( Icon.legacyApis, "legacyApis" )
    , ( Icon.legacyHistorical, "legacyHistorical" )
    , ( Icon.legacyLabelPrinter, "legacyLabelPrinter" )
    , ( Icon.legacyReport, "legacyReport" )
    , ( Icon.legacyRetailerDashboard, "legacyRetailerDashboard" )
    , ( Icon.legacySignOut, "legacySignOut" )
    , ( Icon.list, "list" )
    , ( Icon.loader, "loader" )
    , ( Icon.location, "location" )
    , ( Icon.lock, "lock" )
    , ( Icon.logout, "logout" )
    , ( Icon.map, "map" )
    , ( Icon.mapPinRadius, "mapPinRadius" )
    , ( Icon.mapPin, "mapPin" )
    , ( Icon.messageLock, "messageLock" )
    , ( Icon.messageOTP, "messageOTP" )
    , ( Icon.message, "message" )
    , ( Icon.microphoneMute, "microphoneMute" )
    , ( Icon.microphone, "microphone" )
    , ( Icon.minus, "minus" )
    , ( Icon.move, "move" )
    , ( Icon.notes, "notes" )
    , ( Icon.notepad, "notepad" )
    , ( Icon.notification, "notification" )
    , ( Icon.otp, "otp" )
    , ( Icon.personAssign, "personAssign" )
    , ( Icon.personAssigned, "personAssigned" )
    , ( Icon.personError, "personError" )
    , ( Icon.personUnassign, "personUnassign" )
    , ( Icon.person, "person" )
    , ( Icon.personsAdd, "personsAdd" )
    , ( Icon.persons, "persons" )
    , ( Icon.phoneEndCall, "phoneEndCall" )
    , ( Icon.phoneStartCall, "phoneStartCall" )
    , ( Icon.placeholder, "placeholder" )
    , ( Icon.printerError, "printerError" )
    , ( Icon.printer, "printer" )
    , ( Icon.redo, "redo" )
    , ( Icon.reload, "reload" )
    , ( Icon.remove, "remove" )
    , ( Icon.reporting, "reporting" )
    , ( Icon.return, "return" )
    , ( Icon.scanBarcode, "scanBarcode" )
    , ( Icon.scanGroup, "scanGroup" )
    , ( Icon.scanPackage, "scanPackage" )
    , ( Icon.scanSpace, "scanSpace" )
    , ( Icon.scan, "scan" )
    , ( Icon.search, "search" )
    , ( Icon.settings, "settings" )
    , ( Icon.shelves, "shelves" )
    , ( Icon.sort, "sort" )
    , ( Icon.spaceError, "spaceError" )
    , ( Icon.spaceSearch, "spaceSearch" )
    , ( Icon.space, "space" )
    , ( Icon.speakerMute, "speakerMute" )
    , ( Icon.speaker, "speaker" )
    , ( Icon.store, "store" )
    , ( Icon.tag, "tag" )
    , ( Icon.trash, "trash" )
    , ( Icon.truck, "truck" )
    , ( Icon.undo, "undo" )
    , ( Icon.van, "van" )
    , ( Icon.walk, "walk" )
    , ( Icon.warning, "warning" )
    , ( Icon.waves, "waves" )
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
        , Text.caption label
            |> Text.withOverflow ellipsize
            |> Text.renderElement cfg
            |> Element.el [ Font.center, Element.centerX, Element.width fill, Element.clipX, Element.height (px 14) ]
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
