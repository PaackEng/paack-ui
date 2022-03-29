module UI.Menu exposing
    ( Menu, menu
    , MenuItem, item
    , OpenDirection, openAbove, openBelow
    , itemWithDangerTone, withOpenDirection
    , setVisible
    , renderElement
    )

{-| The `UI.Menu` is a component for rendering dropdown menus.

Following Elm-UI standards, this component is accessible.

A menu can be created and rendered as in the following pipeline:

    Button.cmd ToggleMenu Button.primary
        |> Menu.menu ToggleMenu
            [ Menu.item Download
                (Just Icon.download)
                "Download"
            ]
        |> Menu.renderElement renderConfig


# Building

@docs Menu, menu
@docs MenuItem, item


# Style

@docs OpenDirection, openAbove, openBelow
@docs itemWithDangerTone, withOpenDirection


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
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text
import UI.Utils.ARIA as ARIA


type alias Properties msg =
    { button : Button msg
    , items : List (MenuItem msg)
    , onToggle : msg
    , isVisible : Bool
    , direction : OpenDirection
    }


{-| The `Menu msg` type is used for describing the component for later rendering.
-}
type Menu msg
    = Menu (Properties msg)


{-| The `OpenDirection` is used to determine the direction that the menu will open.
-}
type OpenDirection
    = Above
    | Below


{-| The `MenuItem` is required when assembling the list of menu entries.
-}
type MenuItem msg
    = MenuItem (InternalMenuItem msg)


type alias InternalMenuItem msg =
    { danger : Bool
    , icon : Maybe (String -> Icon)
    , title : String
    , onClick : msg
    }


{-| Constructs a `MenuItem`.

    Button.cmd ToggleMenu Button.primary
        |> Menu.menu ToggleMenu
            [ Menu.item Download
                (Just Icon.download)
                "Download"
            ]
        |> Menu.renderElement renderConfig

-}
item : msg -> Maybe (String -> Icon) -> String -> MenuItem msg
item onToggle icon title =
    MenuItem { onClick = onToggle, icon = icon, title = title, danger = False }


{-| Renders the menu above the button, mostly used on menus that appear on the bottom of the screen.
-}
openAbove : OpenDirection
openAbove =
    Above


{-| Renders the menu bellow the button, this is the default menu direction.
-}
openBelow : OpenDirection
openBelow =
    Below


{-| Sets the color of a `MenuItem`.

    Button.cmd ToggleMenu Button.primary
        |> Menu.menu ToggleMenu
            [ "Delete"
                |> Menu.item Delete (Just Icon.delete)
                |> Menu.itemWithDangerTone
            ]
        |> Menu.renderElement renderConfig

-}
itemWithDangerTone : MenuItem msg -> MenuItem msg
itemWithDangerTone (MenuItem menuItem) =
    MenuItem { menuItem | danger = True }


{-| Defines all the required properties for creating a dropdown menu.

    Button.cmd ToggleMenu Button.primary
        |> Menu.menu ToggleMenu
            [ "Download"
                |> Menu.item Download Icon.download
            , "Delete"
                |> Menu.item Delete Icon.delete
                |> Menu.itemWithDangerTone
            ]
        |> Menu.renderElement renderConfig

-}
menu : msg -> List (MenuItem msg) -> Button msg -> Menu msg
menu onToggle items button =
    Menu
        { button = button
        , items = items
        , onToggle = onToggle
        , isVisible = False
        , direction = Below
        }


{-| Show or hide the menu overlay.

    Menu.setVisible True someMenu

-}
setVisible : Bool -> Menu msg -> Menu msg
setVisible isVisible (Menu state) =
    Menu { state | isVisible = isVisible }


{-| Sets menu direction.

    Menu.withOpenDirection Menu.above someMenu

-}
withOpenDirection : OpenDirection -> Menu msg -> Menu msg
withOpenDirection direction (Menu state) =
    Menu { state | direction = direction }


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Menu msg -> Element msg
renderElement renderConfig (Menu { button, items, onToggle, isVisible, direction }) =
    button
        |> Button.renderElement renderConfig
        |> Element.el
            (Element.alignRight
                :: Element.pointer
                :: Element.alignTop
                :: (if isVisible then
                        let
                            renderOnDirection =
                                case direction of
                                    Above ->
                                        Element.above

                                    Below ->
                                        Element.below
                        in
                        [ onToggle
                            |> renderMenu renderConfig items
                            |> renderOnDirection
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
            , Background.color <| rgb255 255 255 255
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
renderEntry renderConfig ((MenuItem { onClick, title, danger }) as menuItem) =
    let
        ( color, background ) =
            if danger then
                ( Palette.red, Palette.red200 )

            else
                ( Palette.blue, Palette.gray200 )
    in
    Element.row
        (Element.padding 8
            :: Element.alignTop
            :: Element.spacing 5
            :: Element.width Element.fill
            :: Events.onClick onClick
            :: Element.mouseOver
                [ Palette.toBackgroundColor background ]
            :: Border.rounded 3
            :: ARIA.toElementAttributes ARIA.roleButton
        )
        [ renderIcon renderConfig menuItem color
        , Text.body2 title
            |> Text.withColor color
            |> Text.renderElement renderConfig
        ]


renderIcon : RenderConfig -> MenuItem msg -> Palette.Color -> Element msg
renderIcon renderConfig (MenuItem ({ title } as menuItem)) color =
    case menuItem.icon of
        Just icon ->
            title
                |> icon
                |> Icon.withSize Size.extraSmall
                |> Icon.withColor color
                |> Icon.renderElement renderConfig
                |> Element.el [ Element.alignTop ]

        Nothing ->
            Element.text ""
