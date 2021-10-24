module UI.Internal.Nav.LegacyStackHeader exposing (..)

import Element exposing (Element, centerX, fill, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Svg exposing (g, path)
import Svg.Attributes as SvgAttrs
import UI.Icon as Icon
import UI.Internal.Clickable as Clickable
import UI.Internal.Colors as Colors
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text exposing (ellipsize)
import UI.Utils.Action as Action
import UI.Utils.Element as Element exposing (zeroPadding)


type BarButton msg
    = BackButton msg
    | MenuButton msg


view :
    RenderConfig
    -> BarButton msg
    -> Maybe (Action.WithIcon msg)
    -> ( String, Maybe String )
    -> Element msg
view renderConfig barButton _ label =
    Element.row
        [ Element.width fill
        , Element.paddingEach { top = 14, bottom = 4, left = 4, right = 4 }
        , Border.widthEach { zeroPadding | bottom = 1 }
        , Border.color Colors.gray200
        , Background.color Colors.gray800
        ]
        [ leftButtonView renderConfig barButton
        , Element.el [ centerX, width fill ] (labelView renderConfig label)
        , rightButtonView renderConfig barButton
        ]


leftButtonView : RenderConfig -> BarButton msg -> Element msg
leftButtonView renderConfig leftButton =
    case leftButton of
        BackButton msg ->
            { action = Action.DispatchMsg msg
            , label = (localeTerms renderConfig).sidebar.expand
            , icon = Icon.previousContent
            }
                |> renderAction renderConfig

        MenuButton _ ->
            paackLogoNegative


rightButtonView : RenderConfig -> BarButton msg -> Element msg
rightButtonView renderConfig rightButton =
    case rightButton of
        BackButton _ ->
            Element.none

        MenuButton msg ->
            { action = Action.DispatchMsg msg
            , label = (localeTerms renderConfig).sidebar.previous
            , icon = Icon.sandwichMenu
            }
                |> renderAction renderConfig


labelView : RenderConfig -> ( String, Maybe String ) -> Element msg
labelView renderConfig ( title, maybeSubtitle ) =
    (case maybeSubtitle of
        Just subtitle ->
            Text.combination
                [ Text.subtitle2 title
                , Text.caption subtitle
                ]

        Nothing ->
            Text.subtitle2 title
    )
        |> Text.withOverflow ellipsize
        |> Text.withColor Palette.genericWhite
        |> Text.renderElement renderConfig
        |> Element.el [ Element.centerY, Font.center, Element.width fill ]


renderAction : RenderConfig -> Action.WithIcon msg -> Element msg
renderAction renderConfig action =
    action
        |> Action.iconWith
            (Icon.withSize
                Size.medium
                >> Icon.withColor
                    Palette.gray700
            )
        |> Clickable.actionIcon renderConfig
            [ Element.padding 8
            , Font.color Colors.gray700
            ]


paackLogoNegative : Element msg
paackLogoNegative =
    paackLogo "#FFF" 40


paackLogo : String -> Int -> Element msg
paackLogo fillColor size =
    Element.svg "Paack"
        [ SvgAttrs.version "1.2", SvgAttrs.width <| String.fromInt size ++ "px", SvgAttrs.viewBox "44 48 228 212" ]
        [ g [ SvgAttrs.fill fillColor ]
            [ g []
                [ path
                    [ SvgAttrs.d "M41.7 186.2L79 164.7c13.1-7.5 29.2-7.5 42.2 0l37.3 21.6-37.3 21.6c-13.1 7.5-29.2 7.5-42.2 0l-37.3-21.7zM158.6 118.7l37.3-21.6c13.1-7.5 29.2-7.5 42.2 0l37.3 21.6-37.3 21.6c-13.1 7.5-29.2 7.5-42.2 0l-37.3-21.6zM158.6 253.7v-43.1c0-15.1 8.1-29 21.1-36.6l37.3-21.6v43.1c0 15.1-8.1 29-21.1 36.6l-37.3 21.6zM100.1 152.4v-43.1c0-15.1 8.1-29 21.1-36.6l37.3-21.6v43.1c0 15.1-8.1 29-21.1 36.6l-37.3 21.6z" ]
                    []
                ]
            ]
        ]
