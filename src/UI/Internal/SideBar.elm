module UI.Internal.SideBar exposing (desktopColumn)

import Element exposing (Attribute, Element, fill, height, padding, paddingEach, paddingXY, px, scrollbarY, shrink, spacing, width)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Menu as Menu exposing (Menu)
import UI.Internal.Palette as Palette
import UI.Internal.Primitives as Primitives
import UI.Link as Link exposing (Link)
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element



-- Render


desktopColumn : RenderConfig -> Element msg -> Menu msg -> Element msg
desktopColumn cfg page sidebar =
    Element.row [ width fill, height fill ]
        [ view cfg sidebar
        , Element.column
            [ width fill
            , Element.vhHeight 100
            , scrollbarY
            , Element.alignTop
            ]
            [ page ]
        ]



-- Internals


view : RenderConfig -> Menu msg -> Element msg
view cfg (Menu.Menu prop opt) =
    if prop.isExpanded then
        Element.column
            [ height fill
            , width (px 228)
            , padding 12
            , Background.color Palette.gray.lightest
            ]
            [ headerView cfg (prop.toggleMsg False) opt.logo
            , pagesView cfg
                opt.pages
                prop.isExpanded
            , actionsView cfg
                opt.actions
                prop.isExpanded
            ]

    else
        Element.column
            [ height fill
            , width shrink
            , Background.color Palette.gray.lightest
            ]
            [ slimHeaderView cfg (prop.toggleMsg True) opt.logo
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
                |> Element.toRgb
                |> (\color ->
                        { color | alpha = 0.12 }
                   )
                |> Element.fromRgb

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
            |> Text.withColor Text.colorPrimary
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
            |> Text.withColor Text.colorPrimary
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
