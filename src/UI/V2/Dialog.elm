module UI.V2.Dialog exposing
    ( Dialog(..)
    , dialogMap
    , renderElement
    , withBody
    , withButtons
    )

import Element exposing (Element, fill, shrink)
import Element.Border as Border
import UI.Button as Button exposing (Button)
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Palette as PaletteInternal
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.Element as Element


type Dialog msg
    = Dialog Properties (Options msg)


type alias Properties =
    { title : String
    , icon : Icon
    }


type alias Options msg =
    { body : Element msg
    , buttons : List (Button msg)
    }


dialogMap : (a -> b) -> Dialog a -> Dialog b
dialogMap applier (Dialog { title, icon } { body, buttons }) =
    Dialog
        { title = title
        , icon = icon
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
renderElement cfg dialog =
    desktopDialogView cfg dialog


desktopDialogView : RenderConfig -> Dialog msg -> Element msg
desktopDialogView cfg (Dialog { title, icon } { body, buttons }) =
    Element.column
        [ Element.width shrink
        , Element.centerY
        , Element.centerX
        , PaletteInternal.mainBackground
        , Element.padding 32
        , Border.rounded 6
        ]
        [ desktopHeaderRow cfg title icon
        , Element.el
            [ Element.paddingEach
                { top = 8
                , bottom = 0
                , left = 0
                , right =
                    0
                }
            ]
            body
        , Element.row
            [ Element.spacing 16
            , Element.paddingEach { top = 22, left = 0, right = 0, bottom = 0 }
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
