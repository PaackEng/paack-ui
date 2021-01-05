module UI.Internal.DialogV2 exposing (dialogViewV2)

import Element exposing (Element, fill, shrink)
import Element.Border as Border
import Element.Events as Events
import UI.Button as Button exposing (Button)
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Colors exposing (mainBackground, overlayBackground)
import UI.Internal.RenderConfig exposing (RenderConfig)
import UI.Palette as Palette
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text
import UI.Utils.Element as Element exposing (RectangleSides)
import UI.V2.Dialog as Dialog


dialogViewV2 : RenderConfig -> Dialog.Dialog msg -> Element msg
dialogViewV2 cfg ((Dialog.Dialog _ { overlayClickCloseMsg }) as dlg) =
    if RenderConfig.isMobile cfg then
        viewWithOverlay
            { top = 0, bottom = 0, left = 20, right = 20 }
            overlayClickCloseMsg
            (mobileView cfg dlg)

    else
        viewWithOverlay
            { top = 0, bottom = 0, left = 0, right = 0 }
            overlayClickCloseMsg
            (desktopDialogView cfg dlg)


viewWithOverlay : RectangleSides -> Maybe msg -> Element msg -> Element msg
viewWithOverlay padding overlayClickCloseMsg dialogView =
    Element.el
        [ Element.width fill
        , Element.height fill
        , Element.behindContent (blackBlock overlayClickCloseMsg)
        , Element.paddingEach padding
        ]
        dialogView


desktopDialogView : RenderConfig -> Dialog.Dialog msg -> Element msg
desktopDialogView cfg (Dialog.Dialog { title, icon } { body, buttons }) =
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
        , buttonsRow cfg buttons
        ]


buttonsColumn : RenderConfig -> List (Button msg) -> Element msg
buttonsColumn cfg buttons =
    Element.column
        [ Element.spacing 12
        , Element.width fill
        , Element.paddingEach { top = 20, left = 0, right = 0, bottom = 0 }
        ]
    <|
        List.map
            (Button.withSize Size.medium
                >> Button.withWidth Button.widthFull
                >> Button.renderElement cfg
            )
            buttons


buttonsRow : RenderConfig -> List (Button msg) -> Element msg
buttonsRow cfg buttons =
    Element.row
        [ Element.spacing 16
        , Element.paddingEach { top = 24, left = 0, right = 0, bottom = 0 }
        ]
    <|
        List.map
            (Button.withSize Size.medium >> Button.renderElement cfg)
            buttons


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
        , titleText cfg title |> Element.el [ Element.width fill ]
        ]


mobileView : RenderConfig -> Dialog.Dialog msg -> Element msg
mobileView cfg (Dialog.Dialog { title, icon } { body, buttons }) =
    Element.column
        [ mainBackground
        , Element.centerX
        , Element.centerY
        , Element.padding 32
        , Element.spacing 8
        , Border.rounded 6
        ]
        [ mobileHeader cfg title icon
        , body
        , buttonsColumn cfg buttons
        ]


mobileHeader : RenderConfig -> String -> Icon -> Element msg
mobileHeader cfg title icon =
    Element.column
        [ Element.spacing 12 ]
        [ icon
            |> Icon.withColor headerColor
            |> Icon.renderElement cfg
            |> Element.el [ Element.alignLeft ]
        , titleText cfg title
        ]


titleText : RenderConfig -> String -> Element msg
titleText cfg title =
    Text.heading5 title
        |> Text.withColor headerColor
        |> Text.renderElement cfg


headerColor : Palette.Color
headerColor =
    Palette.color Palette.toneGray Palette.brightnessMiddle


{-| Making overlay part of the dialog since it is almost always used with it and
almost all of the major UI frameworks follow this practice.
-}
blackBlock : Maybe msg -> Element msg
blackBlock close =
    Element.el
        [ Element.width fill
        , Element.height fill
        , overlayBackground
        , case close of
            Just msg ->
                Events.onClick msg

            Nothing ->
                Element.width fill
        ]
        Element.none
