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
import UI.Text as Text
import UI.Utils.Element as Element
import UI.V2.Dialog as Dialog


dialogViewV2 : RenderConfig -> Dialog.Dialog msg -> Element msg
dialogViewV2 cfg ((Dialog.Dialog _ { overlayClickCloseMsg }) as dlg) =
    if RenderConfig.isMobile cfg then
        mobileView cfg dlg

    else
        desktopDialogView cfg dlg
            |> Element.el
                [ Element.width fill
                , Element.height fill
                , Element.behindContent (blackBlock overlayClickCloseMsg)
                ]


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


buttonsRow : RenderConfig -> List (Button msg) -> Element msg
buttonsRow cfg buttons =
    Element.row
        [ Element.spacing 16
        , Element.paddingEach { top = 24, left = 0, right = 0, bottom = 0 }
        ]
    <|
        List.map
            (Button.renderElement cfg)
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
mobileView cfg (Dialog.Dialog { title } { body, buttons }) =
    Element.column
        [ Element.width fill
        , Element.height fill
        , Element.alignTop
        , Element.spacing 8
        , mainBackground
        ]
        [ mobileHeaderRow cfg title
        , body
            |> Element.el
                [ Element.width fill
                , Element.paddingEach
                    { top = 0, left = 20, right = 20, bottom = 0 }
                ]
        , buttonsRow cfg buttons
            |> Element.el
                [ Element.paddingEach
                    { left = 20
                    , right = 20
                    , top = 0
                    , bottom = 20
                    }
                ]
        ]


mobileHeaderRow : RenderConfig -> String -> Element msg
mobileHeaderRow cfg title =
    Element.row
        [ Element.width fill
        , Element.padding 0
        ]
        [ titleText cfg
            title
            |> Element.el
                [ Element.paddingEach
                    { top = 40, left = 20, right = 0, bottom = 0 }
                , Element.width fill
                ]
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
