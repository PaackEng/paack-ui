module UI.Menu exposing
    ( Menu, menu
    , MenuItem, item
    , itemWithColor
    , setVisible
    , renderElement
    )

{-| The `UI.Menu` is a component for rendering dropdown menus.

Following Elm-UI standards, this component is accessible.

A menu can be created and rendered as in the following pipeline:

    Button.cmd ToggleMenu Button.primary
        |> Menu.menu ToggleMenu
            [ Menu.item Download
                Icon.download
                "Download"
            ]
        |> Menu.renderElement renderConfig


# Building

@docs Menu, menu
@docs MenuItem, item


# Style

@docs itemWithColor


# Interactive

@docs setVisible


# Rendering

@docs renderElement

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Html.Attributes as HtmlAttrs
import UI.Button as Button exposing (Button)
import UI.Icon as Icon exposing (Icon)
import UI.Palette as Palette exposing (Color)
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text
import UI.Utils.ARIA as ARIA


type alias Properties msg =
    { button : Button msg
    , items : List (MenuItem msg)
    , onToggle : msg
    , isVisible : Bool
    }


{-| The `Menu msg` type is used for describing the component for later rendering.
-}
type Menu msg
    = Menu (Properties msg)


{-| The `MenuItem` is required when assembling the list of menu entries.
-}
type MenuItem msg
    = MenuItem (InternalMenuItem msg)


type alias InternalMenuItem msg =
    { color : Maybe Color
    , icon : String -> Icon
    , title : String
    , onClick : msg
    }


{-| Constructs a `MenuItem`.

    Button.cmd ToggleMenu Button.primary
        |> Menu.menu ToggleMenu
            [ Menu.item Download
                Icon.download
                "Download"
            ]
        |> Menu.renderElement renderConfig

-}
item : msg -> (String -> Icon) -> String -> MenuItem msg
item onToggle icon title =
    MenuItem { onClick = onToggle, icon = icon, title = title, color = Nothing }


{-| Sets the color of a `MenuItem`.

    Button.cmd ToggleMenu Button.primary
        |> Menu.menu ToggleMenu
            [ "Delete"
                |> Menu.item Delete Icon.delete
                |> Menu.itemWithColor Palette.red700
            ]
        |> Menu.renderElement renderConfig

-}
itemWithColor : Color -> MenuItem msg -> MenuItem msg
itemWithColor color (MenuItem menuItem) =
    MenuItem { menuItem | color = Just color }


{-| Defines all the required properties for creating a dropdown menu.

    Button.cmd ToggleMenu Button.primary
        |> Menu.menu ToggleMenu
            [ "Download"
                |> Menu.item Download Icon.download
            , "Delete"
                |> Menu.item Delete Icon.delete
                |> Menu.itemWithColor Palette.red700
            ]
        |> Menu.renderElement renderConfig

-}
menu : msg -> Button msg -> Menu msg
menu onToggle button =
    Menu
        { button = button
        , items = []
        , onToggle = onToggle
        , isVisible = False
        }


{-| Show or hide the menu overlay.

    Menu.setVisible True someMenu

-}
setVisible : Bool -> Menu msg -> Menu msg
setVisible isVisible (Menu state) =
    Menu { state | isVisible = isVisible }


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Menu msg -> Element msg
renderElement renderConfig (Menu { button, items, onToggle, isVisible }) =
    button
        |> Button.renderElement renderConfig
        |> Element.el
            (Element.alignRight
                :: Element.pointer
                :: Element.alignTop
                :: (if isVisible then
                        [ onToggle
                            |> renderMenu renderConfig items
                            |> Element.below
                        ]

                    else
                        []
                   )
            )



-- Internals


renderMenu : RenderConfig -> List (MenuItem msg) -> msg -> Element msg
renderMenu renderConfig items onToggle =
    let
        shadow blur =
            Border.shadow
                { offset = ( 0, 4 )
                , size = 0
                , blur = blur
                , color = Element.rgba 0 0 0 0.04
                }
    in
    items
        |> List.map (renderEntry renderConfig)
        |> Element.column
            [ Element.width <| Element.px 200
            , Element.height Element.fill
            , Element.padding 4
            , Element.alignRight
            , Background.color <| Element.rgb 255 255 255
            , Palette.toBorderColor <| Palette.gray200
            , Border.width 1
            , Border.rounded 4
            , shadow 16
            , shadow 80
            ]
        |> Element.el
            [ Element.none
                |> Element.el
                    [ Element.htmlAttribute <| HtmlAttrs.style "position" "fixed"
                    , Element.htmlAttribute <| HtmlAttrs.style "top" "0"
                    , Element.htmlAttribute <| HtmlAttrs.style "left" "0"
                    , Element.htmlAttribute <| HtmlAttrs.style "width" "100vw"
                    , Element.htmlAttribute <| HtmlAttrs.style "height" "100vh"
                    , Events.onClick onToggle
                    ]
                |> Element.behindContent
            , Element.width Element.fill
            , Element.height Element.fill
            ]


renderEntry : RenderConfig -> MenuItem msg -> Element msg
renderEntry renderConfig (MenuItem { onClick, icon, title, color }) =
    Element.row
        (Element.padding 8
            :: Element.alignTop
            :: Element.spacing 5
            :: Element.width Element.fill
            :: Events.onClick onClick
            :: Element.mouseOver
                [ Palette.toBackgroundColor Palette.gray300 ]
            :: Border.rounded 3
            :: ARIA.toElementAttributes ARIA.roleButton
        )
        [ title
            |> icon
            |> Icon.withSize Size.extraSmall
            |> Icon.withColor (Maybe.withDefault Palette.blue color)
            |> Icon.renderElement renderConfig
            |> Element.el [ Element.alignTop ]
        , Text.body2 title
            |> Text.withColor (Maybe.withDefault Palette.blue color)
            |> Text.renderElement renderConfig
        ]
