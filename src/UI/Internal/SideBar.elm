module UI.Internal.SideBar exposing (desktopColumn, mobileDrawer)

import Element exposing (Attribute, Element, fill, fillPortion, height, padding, paddingEach, paddingXY, px, scrollbarY, shrink, spacing, width)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Menu as Menu exposing (Menu)
import UI.Internal.Nav.StackHeader as StackHeader
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Link as Link exposing (Link)
import UI.Palette as Palette exposing (brightnessLight, brightnessMiddle, toneGray, tonePrimary)
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text
import UI.Utils.ARIA as ARIA
import UI.Utils.Action as Action
import UI.Utils.Element as Element



-- Render


desktopColumn : RenderConfig -> Element msg -> Menu msg -> Element msg
desktopColumn cfg page menu =
    Element.row [ width fill, height fill ]
        [ viewSide cfg False menu
        , Element.el
            [ width fill
            , height fill
            , Element.maxHeightVH 100
            , Element.scrollbarY
            ]
            page
        ]


mobileDrawer :
    RenderConfig
    -> Element msg
    -> Menu msg
    -> ( String, Maybe String )
    -> Maybe ( msg, Maybe (Action.WithIcon msg) )
    -> Element msg
mobileDrawer cfg page menu title maybeStack =
    let
        (Menu.Menu { isExpanded, toggleMsg } _) =
            menu

        content100vh =
            Element.column
                [ width fill
                , height fill
                , Element.maxHeightVH 100
                ]
                [ viewHead cfg menu title maybeStack
                , Element.el [ width fill, height fill, Element.scrollbarY ] page
                ]

        menuView =
            Element.row [ width fill, height fill ]
                [ viewSide cfg True menu
                , Element.el
                    [ Palette.gray.darkest
                        |> Element.colorSetOpacity 0.85
                        |> Background.color
                    , width (fillPortion 25)
                    , height fill
                    , Events.onClick (toggleMsg (not isExpanded))
                    ]
                    Element.none
                ]

        containerWithBar =
            Element.column
                [ width fill
                , height fill
                , Element.inFront menuView
                ]
                [ content100vh ]
    in
    if isExpanded then
        containerWithBar

    else
        content100vh



-- Internals


viewHead : RenderConfig -> Menu msg -> ( String, Maybe String ) -> Maybe ( msg, Maybe (Action.WithIcon msg) ) -> Element msg
viewHead cfg (Menu.Menu prop _) label maybeStack =
    case maybeStack of
        Nothing ->
            StackHeader.view cfg
                (StackHeader.MenuButton (prop.toggleMsg True))
                Nothing
                label

        Just ( goBackMsg, rightButton ) ->
            StackHeader.view cfg
                (StackHeader.BackButton goBackMsg)
                rightButton
                label


viewSide : RenderConfig -> Bool -> Menu msg -> Element msg
viewSide cfg proportional (Menu.Menu prop opt) =
    let
        adaptWidth =
            if prop.isExpanded then
                if proportional then
                    width (fillPortion 75)

                else
                    width (px 228)

            else
                width shrink

        adaptHeader =
            if prop.isExpanded then
                headerView

            else
                slimHeaderView
    in
    Element.column
        [ height fill
        , adaptWidth
        , Background.color Palette.gray.lightest
        ]
        [ adaptHeader cfg (prop.toggleMsg (not prop.isExpanded)) opt.logo
        , pagesView cfg
            opt.pages
            prop.isExpanded
        , actionsView cfg
            opt.actions
            prop.isExpanded
        ]


headerView : RenderConfig -> msg -> Maybe (Menu.Logo msg) -> Element msg
headerView cfg toggleMsg logo =
    let
        attr =
            [ paddingEach
                { top = 20
                , left = 32
                , right = 8
                , bottom = 96
                }
            , width fill
            , height (px (60 + 96))
            ]

        logoEl =
            case logo of
                Just { hint, body } ->
                    Element.column
                        ((ARIA.toElementAttributes <| ARIA.roleImage hint)
                            ++ [ paddingEach { top = 12, left = 0, right = 20, bottom = 0 }
                               , width fill
                               ]
                        )
                        [ body ]

                Nothing ->
                    Element.none

        closeButton =
            (cfg |> localeTerms >> .sidebar >> .collapse)
                |> Icon.close
                |> Icon.withSize Size.small
                |> Icon.withColor (Palette.color toneGray brightnessLight)
                |> Icon.renderElement cfg
                |> Element.el (headerButtonAttr toggleMsg)
    in
    Element.row attr
        [ logoEl
        , closeButton
        ]


slimHeaderView : RenderConfig -> msg -> Maybe (Menu.Logo msg) -> Element msg
slimHeaderView cfg toggleMsg _ =
    Element.column [ height (px (72 + 48)) ]
        [ (cfg |> localeTerms >> .sidebar >> .expand)
            |> Icon.sandwichMenu
            |> Icon.withSize Size.small
            |> Icon.renderElement cfg
            |> Element.el (headerButtonAttr toggleMsg)
        ]


headerButtonAttr : msg -> List (Attribute msg)
headerButtonAttr toggleMsg =
    [ Events.onClick toggleMsg
    , Element.pointer
    , Element.centerY
    ]
        ++ ARIA.toElementAttributes ARIA.roleButton


pagesView : RenderConfig -> List Menu.Page -> Bool -> Element msg
pagesView cfg pages navExpanded =
    let
        item { labeledIcon, link, isCurrent } =
            if navExpanded then
                pageItem cfg labeledIcon link isCurrent

            else
                slimPageItem cfg labeledIcon link isCurrent

        paddingAttr =
            if navExpanded then
                paddingEach
                    { top = 0
                    , left = 4
                    , right = 7
                    , bottom = 0
                    }

            else
                padding 0

        spacingAttr =
            if navExpanded then
                spacing 14

            else
                spacing 12

        attrs =
            [ height fill
            , width fill
            , spacingAttr
            , paddingAttr
            ]
    in
    pages
        |> List.map item
        |> Element.column attrs


actionsView : RenderConfig -> List (Menu.Action msg) -> Bool -> Element msg
actionsView cfg actions navExpanded =
    actions
        |> List.map
            (\{ labeledIcon, action } ->
                if navExpanded then
                    actionItem cfg labeledIcon action

                else
                    slimActionItem cfg labeledIcon action
            )
        |> Element.column
            [ height shrink
            , width fill
            , spacing 8
            , if navExpanded then
                paddingEach
                    { top = 0
                    , left = 4
                    , right = 7
                    , bottom = 16
                    }

              else
                paddingEach
                    { top = 0
                    , left = 0
                    , right = 0
                    , bottom = 16
                    }
            ]


pageItem : RenderConfig -> Icon -> Link -> Bool -> Element msg
pageItem cfg icon link isSelected =
    let
        baseAttrs =
            [ width fill
            , paddingXY 4 0
            , Primitives.defaultRoundedBorders
            , spacing 4
            ]

        selectedColor =
            Palette.primary.darkest
                |> Element.colorSetOpacity 0.12

        attrs =
            if isSelected then
                Background.color selectedColor
                    :: baseAttrs

            else
                baseAttrs

        textColor =
            Palette.color tonePrimary brightnessMiddle
    in
    Element.row attrs
        [ icon
            |> Icon.withSize Size.small
            |> Icon.withColor textColor
            |> Icon.renderElement cfg
            |> Element.el iconAttr
        , Icon.getHint icon
            |> Text.body1
            |> Text.withColor textColor
            |> Text.renderElement cfg
        ]
        |> Link.wrapElement cfg [ width fill ] link


slimPageItem : RenderConfig -> Icon -> Link -> Bool -> Element msg
slimPageItem cfg icon link isSelected =
    icon
        |> Icon.withSize Size.small
        |> Icon.withColor (slimIconColor isSelected)
        |> Icon.renderElement cfg
        |> Element.el slimIconAttr
        |> Link.wrapElement cfg [] link


actionItem : RenderConfig -> Icon -> msg -> Element msg
actionItem cfg icon msg =
    let
        attrs =
            [ width fill
            , spacing 4
            , Element.pointer
            , Events.onClick msg
            ]
                ++ ARIA.toElementAttributes ARIA.roleButton
    in
    Element.row attrs
        [ icon
            |> Icon.withSize Size.small
            |> Icon.withColor (Palette.color tonePrimary brightnessMiddle)
            |> Icon.renderElement cfg
            |> Element.el iconAttr
        , Icon.getHint icon
            |> Text.body1
            |> Text.withColor (Palette.color tonePrimary brightnessMiddle)
            |> Text.renderElement cfg
        ]


slimActionItem : RenderConfig -> Icon -> msg -> Element msg
slimActionItem cfg icon msg =
    icon
        |> Icon.withSize Size.small
        |> Icon.withColor (slimIconColor True)
        |> Icon.renderElement cfg
        |> Element.el
            (slimIconAttr
                ++ [ Element.pointer
                   , Events.onClick msg
                   ]
                ++ ARIA.toElementAttributes ARIA.roleButton
            )


iconAttr : List (Attribute msg)
iconAttr =
    [ width (px 32)
    , paddingXY 0 6
    , Font.center
    ]


slimIconAttr : List (Attribute msg)
slimIconAttr =
    [ width (px 48)
    , Font.center
    ]


slimIconColor : Bool -> Palette.Color
slimIconColor isSelected =
    if isSelected then
        Palette.color tonePrimary brightnessMiddle

    else
        Palette.color tonePrimary brightnessMiddle
            |> Palette.withAlpha 0.4
