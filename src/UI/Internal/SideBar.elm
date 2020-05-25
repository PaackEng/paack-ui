module UI.Internal.SideBar exposing (desktopColumn, mobileDrawer)

import Element exposing (Attribute, Element, fill, fillPortion, height, padding, paddingEach, paddingXY, px, scrollbarY, shrink, spacing, width)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import UI.Button as Button exposing (Button)
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Menu as Menu exposing (Menu)
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Link as Link exposing (Link)
import UI.Palette as Palette exposing (brightnessMiddle, tonePrimary)
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.ARIA as ARIA
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
    -> String
    -> Maybe ( msg, List (Button msg) )
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


viewHead : RenderConfig -> Menu msg -> String -> Maybe ( msg, List (Button msg) ) -> Element msg
viewHead cfg (Menu.Menu prop opt) title maybeStack =
    let
        mobileHeadSandwich =
            Icon.sandwichMenu "Expand sidebar"
                |> Icon.toEl cfg
                |> Element.el (headerButtonAttr (prop.toggleMsg True) 48 20)

        mobileHeadGoBack msg =
            Icon.backwardContent "Go back"
                |> Icon.toEl cfg
                |> Element.el (headerButtonAttr msg 48 20)
    in
    case maybeStack of
        Nothing ->
            Element.row
                [ width fill
                , Element.inFront mobileHeadSandwich
                ]
                [ Element.el
                    [ Element.width fill, Font.center, padding 20 ]
                    (Text.heading5 title |> Text.toEl cfg)
                ]

        Just ( goBackMsg, stackButtons ) ->
            Element.row [ width fill, spacing 8, paddingEach { left = 4, right = 12, top = 0, bottom = 0 } ] <|
                [ mobileHeadGoBack goBackMsg
                , Element.el
                    [ Element.width fill, Element.centerY ]
                    (Text.heading5 title |> Text.toEl cfg)
                ]
                    ++ List.map (Button.toEl cfg) stackButtons


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
                    [ body ]
                        -- TODO: Add Hint
                        |> Element.column
                            [ paddingEach { top = 12, left = 0, right = 20, bottom = 0 }
                            , width fill
                            ]

                Nothing ->
                    Element.none

        closeButton =
            Icon.close "Minimize sidebar"
                |> Icon.toEl cfg
                |> Element.el (headerButtonAttr toggleMsg 40 10)
    in
    Element.row attr
        [ logoEl
        , closeButton
        ]


slimHeaderView : RenderConfig -> msg -> Maybe (Menu.Logo msg) -> Element msg
slimHeaderView cfg toggleMsg logo =
    Element.column [ height (px (72 + 48)) ]
        [ Icon.sandwichMenu "Expand sidebar"
            |> Icon.toEl cfg
            |> Element.el (headerButtonAttr toggleMsg 48 14)
        ]


headerButtonAttr : msg -> Int -> Int -> List (Attribute msg)
headerButtonAttr toggleMsg boxWidth padY =
    [ width (px boxWidth)
    , paddingXY 0 padY
    , Font.center
    , Events.onClick toggleMsg
    , Element.pointer
    , ARIA.roleAttr ARIA.roleButton -- TODO: Add to tests
    ]


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
                spacing 8

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
                    , bottom = 8
                    }

              else
                paddingEach
                    { top = 0
                    , left = 0
                    , right = 0
                    , bottom = 8
                    }
            ]


pageItem : RenderConfig -> Icon -> Link -> Bool -> Element msg
pageItem cfg icon link isSelected =
    let
        baseAttrs =
            [ width fill
            , paddingXY 4 0
            , Primitives.roundedBorders
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
    in
    Element.row attrs
        [ Icon.toEl cfg icon
            |> Element.el iconAttr
        , Icon.getHint icon
            |> Text.body1
            |> Text.withColor (Palette.color tonePrimary brightnessMiddle)
            |> Text.toEl cfg
        ]
        |> Link.packEl cfg [ width fill ] link


slimPageItem : RenderConfig -> Icon -> Link -> Bool -> Element msg
slimPageItem cfg icon link isSelected =
    Icon.toEl cfg icon
        |> Element.el (slimIconAttr isSelected)
        |> Link.packEl cfg [] link


actionItem : RenderConfig -> Icon -> msg -> Element msg
actionItem cfg icon msg =
    let
        attrs =
            [ width fill
            , spacing 4
            , Element.pointer
            , ARIA.roleAttr ARIA.roleButton -- TODO: Add to tests
            , Events.onClick msg
            ]
    in
    Element.row attrs
        [ Icon.toEl cfg icon
            |> Element.el iconAttr
        , Icon.getHint icon
            |> Text.body1
            |> Text.withColor (Palette.color tonePrimary brightnessMiddle)
            |> Text.toEl cfg
        ]


slimActionItem : RenderConfig -> Icon -> msg -> Element msg
slimActionItem cfg icon msg =
    let
        attr =
            slimIconAttr True
                ++ [ Element.pointer
                   , ARIA.roleAttr ARIA.roleButton -- TODO: Check on tests
                   , Events.onClick msg
                   ]
    in
    Icon.toEl cfg icon
        |> Element.el attr


iconAttr : List (Attribute msg)
iconAttr =
    [ width (px 32)
    , paddingXY 0 6
    , Font.center
    , Font.color Palette.primary.middle -- TODO: Implement Icon.withColor
    ]


slimIconAttr : Bool -> List (Attribute msg)
slimIconAttr isSelected =
    [ width (px 48)
    , paddingXY 0 14
    , Font.center
    , Font.color <|
        -- TODO: Implement Icon.withColor
        if isSelected then
            Palette.primary.middle

        else
            Palette.primary.middle
                |> Element.toRgb
                |> (\color ->
                        { color | alpha = 0.4 }
                   )
                |> Element.fromRgb
    ]
