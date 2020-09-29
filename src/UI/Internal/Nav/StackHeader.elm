module UI.Internal.Nav.StackHeader exposing (LeftButton(..), view)

import Element exposing (Element, fill, minimum, shrink)
import Element.Border as Border
import Element.Font as Font
import UI.Button as Button exposing (Button)
import UI.Icon as Icon exposing (Icon)
import UI.Internal.Button as Button
import UI.Internal.Palette as Palette
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Palette as Palette exposing (brightnessMiddle, toneGray)
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text exposing (ellipsize)
import UI.Utils.Element as Element exposing (zeroPadding)


type LeftButton msg
    = BackButton msg
    | MenuButton msg


view :
    RenderConfig
    -> LeftButton msg
    -> Maybe (Button msg)
    -> ( String, Maybe String )
    -> Element msg
view renderConfig leftButton rightButton label =
    Element.row
        [ Element.width fill
        , Element.paddingEach { top = 14, bottom = 4, left = 4, right = 4 }
        , Border.widthEach { zeroPadding | bottom = 1 }
        , Border.color Palette.gray.lightest
        ]
        [ leftButtonView renderConfig leftButton
        , labelView renderConfig label
        , rightButtonView renderConfig rightButton
        ]


leftButtonView : RenderConfig -> LeftButton msg -> Element msg
leftButtonView renderConfig leftButton =
    case leftButton of
        BackButton msg ->
            (localeTerms renderConfig).sidebar.expand
                |> Icon.sandwichMenu
                |> iconToButton renderConfig msg

        MenuButton msg ->
            (localeTerms renderConfig).sidebar.previous
                |> Icon.previousContent
                |> iconToButton renderConfig msg


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


rightButtonView : RenderConfig -> Maybe (Button msg) -> Element msg
rightButtonView renderConfig maybeRightButton =
    case maybeRightButton of
        Just rightButton ->
            rightButton
                |> Button.withSize Size.medium
                |> Button.renderUnstyled renderConfig [ Element.padding 8 ]

        Nothing ->
            Element.el
                [ Element.width (shrink |> minimum 32)
                , Element.height (shrink |> minimum 32)
                , Element.padding 8
                ]
                Element.none


iconToButton : RenderConfig -> msg -> Icon -> Element msg
iconToButton renderConfig msg icon =
    icon
        |> Icon.withColor (Palette.color toneGray brightnessMiddle)
        |> Button.fromIcon
        |> Button.cmd msg Button.clear
        |> Button.withSize Size.medium
        |> Button.renderUnstyled renderConfig [ Element.padding 8 ]
