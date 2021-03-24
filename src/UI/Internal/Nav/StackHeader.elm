module UI.Internal.Nav.StackHeader exposing (LeftButton(..), view)

import Element exposing (Element, fill, minimum, shrink)
import Element.Border as Border
import Element.Font as Font
import UI.Icon as Icon
import UI.Internal.Clickable as Clickable
import UI.Internal.Colors as Colors
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Palette as Palette exposing (brightnessMiddle, toneGray)
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text exposing (ellipsize)
import UI.Utils.Action as Action
import UI.Utils.Element exposing (zeroPadding)


type LeftButton msg
    = BackButton msg
    | MenuButton msg


view :
    RenderConfig
    -> LeftButton msg
    -> Maybe (Action.WithIcon msg)
    -> ( String, Maybe String )
    -> Element msg
view renderConfig leftButton rightAction label =
    Element.row
        [ Element.width fill
        , Element.paddingEach { top = 14, bottom = 4, left = 4, right = 4 }
        , Border.widthEach { zeroPadding | bottom = 1 }
        , Border.color Colors.gray.lightest
        ]
        [ leftButtonView renderConfig leftButton
        , labelView renderConfig label
        , rightActionView renderConfig rightAction
        ]


leftButtonView : RenderConfig -> LeftButton msg -> Element msg
leftButtonView renderConfig leftButton =
    case leftButton of
        BackButton msg ->
            { action = Action.DispatchMsg msg
            , label = (localeTerms renderConfig).sidebar.expand
            , icon = Icon.previousContent
            }
                |> renderAction renderConfig

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
        |> Text.renderElement renderConfig
        |> Element.el [ Element.centerY, Font.center, Element.width fill ]


rightActionView : RenderConfig -> Maybe (Action.WithIcon msg) -> Element msg
rightActionView renderConfig maybeRightAction =
    case maybeRightAction of
        Just rightAction ->
            renderAction renderConfig rightAction

        Nothing ->
            Element.el
                [ Element.width (shrink |> minimum 32)
                , Element.height (shrink |> minimum 32)
                , Element.padding 8
                ]
                Element.none


renderAction : RenderConfig -> Action.WithIcon msg -> Element msg
renderAction renderConfig action =
    action
        |> Action.iconWith
            (Icon.withSize
                Size.medium
                >> Icon.withColor
                    (Palette.color toneGray brightnessMiddle)
            )
        |> Clickable.actionIcon renderConfig
            [ Element.padding 8
            , Font.color Colors.gray.middle
            ]
