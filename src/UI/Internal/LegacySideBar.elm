module UI.Internal.LegacySideBar exposing (..)

import Element exposing (Attribute, Element, alignRight, fill, height, padding, paddingEach, paddingXY, shrink, spacing, width)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Colors as Colors
import UI.Internal.Menu as Menu exposing (Menu)
import UI.Internal.Nav.LegacyStackHeader as LegacyStackHeader
import UI.Internal.Primitives as Primitives
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Link as Link exposing (Link)
import UI.Palette as Palette
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text
import UI.Utils.ARIA as ARIA
import UI.Utils.Action as Action
import UI.Utils.Element as Element



-- Render


type alias SidebarConfig =
    { proportional : Bool
    , persistent : Bool
    }


desktopPersistent : RenderConfig -> Element msg -> Menu msg -> Element msg
desktopPersistent cfg page (Menu.Menu prop opt) =
    Element.row
        [ width fill
        , height fill
        ]
        [ viewSide cfg
            { proportional = False, persistent = True }
            (Menu.Menu { prop | isExpanded = False } opt)
        , Element.el
            [ width fill
            , height fill
            , Element.maxHeightVH 100
            , Element.scrollbarY
            ]
            page
        ]


mobile :
    RenderConfig
    -> Element msg
    -> Menu msg
    -> ( String, Maybe String )
    -> Maybe ( msg, Maybe (Action.WithIcon msg) )
    -> Element msg
mobile cfg page menu title maybeStack =
    let
        (Menu.Menu { isExpanded } _) =
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
    in
    if isExpanded then
        let
            menuView =
                Element.row [ width fill ]
                    [ viewSide cfg { proportional = True, persistent = True } menu
                    ]

            containerWithBar =
                Element.column
                    [ width fill
                    , height fill
                    , Element.inFront menuView
                    ]
                    [ content100vh ]
        in
        containerWithBar

    else
        content100vh



-- Internals


viewHead : RenderConfig -> Menu msg -> ( String, Maybe String ) -> Maybe ( msg, Maybe (Action.WithIcon msg) ) -> Element msg
viewHead cfg (Menu.Menu prop _) label maybeStack =
    case maybeStack of
        Nothing ->
            LegacyStackHeader.view cfg
                (LegacyStackHeader.MenuButton (prop.toggleMsg True))
                Nothing
                label

        Just ( goBackMsg, rightButton ) ->
            LegacyStackHeader.view cfg
                (LegacyStackHeader.BackButton goBackMsg)
                rightButton
                label


viewSide : RenderConfig -> SidebarConfig -> Menu msg -> Element msg
viewSide cfg _ (Menu.Menu prop opt) =
    let
        adaptWidth =
            if RenderConfig.isMobile cfg then
                width fill

            else
                width shrink

        header =
            if prop.isExpanded then
                let
                    toggleMsg =
                        prop.toggleMsg (not prop.isExpanded)
                in
                headerView cfg toggleMsg opt.logo

            else
                LegacyStackHeader.paackLogoNegative
    in
    Element.column
        [ height fill
        , adaptWidth
        , if prop.isExpanded then
            paddingXY 16 24

          else
            paddingXY 6 22
        , Background.color Colors.gray800
        , spacing 36
        ]
        [ Element.column [ width fill ]
            [ Element.el [ paddingEach { top = 0, right = 0, bottom = 60, left = 0 }, width fill ] header
            , pagesView cfg
                opt.pages
                prop
            , Element.el
                [ width fill
                , if prop.isExpanded then
                    paddingEach { top = 8, right = 0, bottom = 0, left = 12 }

                  else
                    paddingEach { top = 36, right = 0, bottom = 0, left = 0 }
                ]
                (actionsView cfg
                    opt.actions
                    prop.isExpanded
                )
            ]
        ]


headerView : RenderConfig -> msg -> Maybe (Menu.Logo msg) -> Element msg
headerView cfg toggleMsg _ =
    let
        attr =
            [ paddingEach
                { top = 0
                , left = 8
                , right = 8
                , bottom = 0
                }
            , width fill
            ]

        hamburgerButton =
            (cfg |> localeTerms >> .sidebar >> .collapse)
                |> Icon.sandwichMenu
                |> Icon.withSize Size.small
                |> Icon.withColor Palette.gray600
                |> Icon.renderElement cfg
                |> Element.el (headerButtonAttr toggleMsg)
    in
    Element.row attr
        [ LegacyStackHeader.paackLogoNegative
        , Element.el [ alignRight ] hamburgerButton
        ]


headerButtonAttr : msg -> List (Attribute msg)
headerButtonAttr toggleMsg =
    Events.onClick toggleMsg
        :: Element.pointer
        :: Element.centerY
        :: Element.alignRight
        :: ARIA.toElementAttributes ARIA.roleButton


pagesView : RenderConfig -> List Menu.Page -> Menu.Properties msg -> Element msg
pagesView cfg pages props =
    let
        item { labeledIcon, link, isCurrent } =
            if props.isExpanded then
                pageItem cfg labeledIcon link isCurrent

            else
                slimPageItem cfg labeledIcon link isCurrent

        spacingAttr =
            spacing <|
                if props.isExpanded then
                    8

                else
                    36

        attrs =
            [ height fill
            , width fill
            , spacingAttr
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
            ]


selectedItemOutline : Bool -> List (Element msg) -> Element msg
selectedItemOutline isSelected =
    let
        baseAttrs =
            [ width fill
            , Primitives.defaultRoundedBorders
            , spacing 8
            , padding 8
            ]

        attrs =
            if isSelected then
                let
                    selectedColor =
                        Palette.gray800 |> Palette.toElementColor
                in
                Background.color selectedColor
                    :: baseAttrs

            else
                baseAttrs
    in
    Element.row attrs


pageItem : RenderConfig -> Icon -> Link -> Bool -> Element msg
pageItem cfg icon link isSelected =
    let
        textColor =
            Palette.genericWhite
    in
    selectedItemOutline
        isSelected
        [ icon
            |> Icon.withSize Size.small
            |> Icon.withColor textColor
            |> Icon.renderElement cfg
            |> Element.el [ Font.center ]
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
        |> Icon.withColor Palette.genericWhite
        |> Icon.renderElement cfg
        |> Element.el [ Font.center ]
        |> List.singleton
        |> selectedItemOutline isSelected
        |> Link.wrapElement cfg [] link


actionItem : RenderConfig -> Icon -> msg -> Element msg
actionItem cfg icon msg =
    let
        attrs =
            width fill
                :: spacing 4
                :: Element.pointer
                :: Events.onClick msg
                :: ARIA.toElementAttributes ARIA.roleButton
    in
    Element.row attrs
        [ icon
            |> Icon.withSize Size.small
            |> Icon.withColor Palette.genericWhite
            |> Icon.renderElement cfg
            |> Element.el [ Font.center ]
        , Icon.getHint icon
            |> Text.body1
            |> Text.withColor Palette.genericWhite
            |> Text.renderElement cfg
        ]


slimActionItem : RenderConfig -> Icon -> msg -> Element msg
slimActionItem cfg icon msg =
    icon
        |> Icon.withSize Size.small
        |> Icon.withColor Palette.genericWhite
        |> Icon.renderElement cfg
        |> Element.el
            (Element.pointer
                :: Events.onClick msg
                :: Element.centerX
                :: Font.center
                :: ARIA.toElementAttributes ARIA.roleButton
            )
