module UI.Internal.Dialog exposing (Dialog, dialogMap, view)

import Element exposing (Element, fill, px, shrink)
import Element.Events as Events
import UI.Icon as Icon
import UI.Internal.Palette as Palette
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Palette as Palette exposing (brightnessLight, toneGray)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text
import UI.Utils.ARIA as ARIA exposing (roleButton)
import UI.Utils.Element as Element exposing (RectangleSides)


type alias Dialog msg =
    { title : String
    , close : msg
    , body : Element msg
    }


dialogMap : (a -> b) -> Dialog a -> Dialog b
dialogMap applier data =
    { title = data.title
    , close = applier data.close
    , body = Element.map applier data.body
    }


view : RenderConfig -> Dialog msg -> Element msg
view cfg dialog =
    if RenderConfig.isMobile cfg then
        mobileView cfg dialog

    else
        desktopView cfg dialog


desktopView : RenderConfig -> Dialog msg -> Element msg
desktopView cfg dialog =
    -- Desktop has a black background
    desktopDialogView cfg dialog
        |> Element.el
            [ Element.width fill
            , Element.height fill
            , Element.behindContent (blackBlock dialog.close)
            ]


desktopDialogView : RenderConfig -> Dialog msg -> Element msg
desktopDialogView cfg { title, body, close } =
    Element.el
        [ Element.width shrink
        , Element.centerY
        , Element.centerX
        , Palette.mainBackground
        , Element.above
            (desktopHeaderRow cfg close title)
        , Element.paddingEach
            { top = 8, left = 32, right = 32, bottom = 32 }
        ]
        body


desktopHeaderRow : RenderConfig -> msg -> String -> Element msg
desktopHeaderRow cfg close title =
    Element.row
        [ Element.width fill
        , Element.paddingEach { top = 12, right = 12, left = 0, bottom = 0 }
        , Palette.mainBackground
        ]
        [ titleText cfg
            { top = 20, left = 32, right = 0, bottom = 0 }
            title
        , closeButton cfg close
        ]


mobileView : RenderConfig -> Dialog msg -> Element msg
mobileView cfg { title, body, close } =
    Element.column
        [ Element.width fill
        , Element.height fill
        , Element.alignTop
        , Element.spacing 8
        , Palette.mainBackground
        ]
        [ mobileHeaderRow cfg close title
        , body
            |> Element.el
                [ Element.width fill
                , Element.paddingEach
                    { top = 0, left = 20, right = 20, bottom = 20 }
                ]
        ]


mobileHeaderRow : RenderConfig -> msg -> String -> Element msg
mobileHeaderRow cfg close title =
    Element.row
        [ Element.width fill
        , Element.padding 0
        ]
        [ titleText cfg
            { top = 40, left = 20, right = 0, bottom = 0 }
            title
        , closeButton cfg close
        ]


titleText : RenderConfig -> RectangleSides -> String -> Element msg
titleText cfg padding title =
    Text.heading5 title
        |> Text.renderElement cfg
        |> Element.el
            [ Element.width (px 400)
            , Element.paddingEach padding
            , Element.alignTop
            ]


closeButton : RenderConfig -> msg -> Element msg
closeButton cfg close =
    (cfg |> localeTerms >> .dialog >> .close)
        |> Icon.close
        |> Icon.withSize Size.small
        |> Icon.withColor (Palette.color Palette.toneGray Palette.brightnessLight)
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


blackBlock : msg -> Element msg
blackBlock close =
    Element.el
        [ Element.width fill
        , Element.height fill
        , Palette.overlayBackground
        , Events.onClick close
        ]
        Element.none
