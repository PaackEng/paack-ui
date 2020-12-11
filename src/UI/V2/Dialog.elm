module UI.V2.Dialog exposing
    ( Dialog(..), dialog
    , withBody, withButtons
    , renderElement
    , map
    )

{-| The `UI.V2.Dialog` is a component for displaying dialogs and modals.

User must specify a title, an icon to be displayed in title and a close message
to construct it. Body and buttons can be specified optionally as in the
following pipeline:

    dialog "Title" Icon.warning closeMsg
        |> withBody ("Body text" |> Text.body2 |> Text.renderElement cfg)
        |> withButtons buttons


# Building

@docs Dialog, dialog


# Content

@docs withBody, withButtons


# Rendering

@docs renderElement


# Component handling

@docs map

-}

import Element exposing (Element, fill, shrink)
import Element.Border as Border
import Element.Events as Events
import UI.Button as Button exposing (Button)
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Colors exposing (mainBackground)
import UI.Internal.RenderConfig exposing (RenderConfig, localeTerms)
import UI.Palette as Palette
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text
import UI.Utils.ARIA as ARIA exposing (roleButton)
import UI.Utils.Element as Element



-- Building


{-| The `Dialog msg` type is used for describing the component for later
rendering.
-}
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


{-| Constructs a dialog by receiving its title, icon in the title and a close
message.
-}
dialog : String -> Icon -> msg -> Dialog msg
dialog title icon closeMsg =
    Dialog (Properties title icon closeMsg) (Options Element.none [])



-- Component handling


{-| Transforms the message produced by the component.
-}
map : (a -> b) -> Dialog a -> Dialog b
map applier (Dialog { title, icon, close } { body, buttons }) =
    Dialog
        { title = title
        , icon = icon
        , close = applier close
        }
        { body = Element.map applier body
        , buttons = List.map (Button.map applier) buttons
        }



-- Content


{-| With `Dialog.withBody` you can specify the body of the dialog.
**Note**: By default, the body is `Element.none`
-}
withBody : Element msg -> Dialog msg -> Dialog msg
withBody body (Dialog props options) =
    Dialog props { options | body = body }


{-| With `Dialog.withButtons` you can specify the buttons for the footer of the
dialog.
**Note**: By default, the buttons are an empty list
-}
withButtons : List (Button msg) -> Dialog msg -> Dialog msg
withButtons buttons (Dialog props options) =
    Dialog props { options | buttons = buttons }



-- Rendering


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Dialog msg -> Element msg
renderElement cfg dlg =
    if RenderConfig.isMobile cfg then
        mobileView cfg dlg

    else
        desktopDialogView cfg dlg



-- Internal


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


mobileView : RenderConfig -> Dialog msg -> Element msg
mobileView cfg (Dialog { title, close } { body, buttons }) =
    Element.column
        [ Element.width fill
        , Element.height fill
        , Element.alignTop
        , Element.spacing 8
        , mainBackground
        ]
        [ mobileHeaderRow cfg close title
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


mobileHeaderRow : RenderConfig -> msg -> String -> Element msg
mobileHeaderRow cfg close title =
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
        , closeButton cfg close
        ]


closeButton : RenderConfig -> msg -> Element msg
closeButton cfg close =
    (cfg |> localeTerms >> .dialog >> .close)
        |> Icon.close
        |> Icon.withSize Size.small
        |> Icon.withColor
            (Palette.color Palette.toneGray Palette.brightnessLight)
        |> Icon.renderElement cfg
        |> Element.el
            (ARIA.toElementAttributes ARIA.roleButton
                ++ [ Events.onClick close
                   , Element.pointer
                   , Element.padding 12
                   , Element.height shrink
                   , Element.alignTop
                   ]
            )


titleText : RenderConfig -> String -> Element msg
titleText cfg title =
    Text.heading5 title
        |> Text.withColor headerColor
        |> Text.renderElement cfg


headerColor : Palette.Color
headerColor =
    Palette.color Palette.toneGray Palette.brightnessMiddle
