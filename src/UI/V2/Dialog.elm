module UI.V2.Dialog exposing
    ( Dialog(..)
    , dialog
    , dialogMap
    , renderElement
    , withBody
    , withButtons
    )

import Element exposing (Element, fill, shrink)
import Element.Border as Border
import UI.Button as Button exposing (Button)
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Colors exposing (mainBackground)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.Element as Element


type Dialog msg
    = Dialog (Properties msg) (Options msg)


type alias Properties msg =
    { title : String
    , icon : Icon
    , close : msg
    }


type alias Options msg =
    { body : Element msg
    , buttons : List (Button msg)
    }


dialog : String -> Icon -> msg -> Dialog msg
dialog title icon closeMsg =
    Dialog (Properties title icon closeMsg) (Options Element.none [])


dialogMap : (a -> b) -> Dialog a -> Dialog b
dialogMap applier (Dialog { title, icon, close } { body, buttons }) =
    Dialog
        { title = title
        , icon = icon
        , close = applier close
        }
        { body = Element.map applier body
        , buttons = List.map (Button.map applier) buttons
        }


withBody : Element msg -> Dialog msg -> Dialog msg
withBody body (Dialog props options) =
    Dialog props { options | body = body }


withButtons : List (Button msg) -> Dialog msg -> Dialog msg
withButtons buttons (Dialog props options) =
    Dialog props { options | buttons = buttons }


renderElement : RenderConfig -> Dialog msg -> Element msg
renderElement cfg dlg =
    desktopDialogView cfg dlg


desktopDialogView : RenderConfig -> Dialog msg -> Element msg
desktopDialogView cfg (Dialog { title, icon } { body, buttons }) =
    Element.column
        [ Element.width shrink
        , Element.centerY
        , Element.centerX
        , mainBackground
        , Element.paddingEach
            { top = 0
            , right = 32
            , bottom = 32
            , left = 32
            }
        , Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomLeft = 6
            , bottomRight = 6
            }
        , Element.above
            (desktopHeaderRow cfg title icon)
        ]
        [ body
        , Element.row
            [ Element.spacing 16
            , Element.paddingEach { top = 24, left = 0, right = 0, bottom = 0 }
            ]
          <|
            List.map
                (Button.renderElement cfg)
                buttons
        ]


desktopHeaderRow : RenderConfig -> String -> Icon -> Element msg
desktopHeaderRow cfg title icon =
    Element.row
        [ Element.spacing 12
        , Element.width fill
        , Element.paddingEach { top = 32, bottom = 8, right = 32, left = 32 }
        , mainBackground
        , Border.roundEach
            { topLeft = 6
            , topRight = 6
            , bottomLeft = 0
            , bottomRight = 0
            }
        ]
        [ icon
            |> Icon.withColor headerColor
            |> Icon.renderElement cfg
        , titleText cfg title
        ]


titleText : RenderConfig -> String -> Element msg
titleText cfg title =
    Text.heading5 title
        |> Text.withColor headerColor
        |> Text.renderElement cfg
        |> Element.el
            [ Element.width fill
            ]


headerColor : Palette.Color
headerColor =
    Palette.color Palette.toneGray Palette.brightnessMiddle
