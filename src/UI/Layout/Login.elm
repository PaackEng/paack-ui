module UI.Layout.Login exposing (view)

import Element exposing (Element, fill, maximum, minimum, shrink)
import UI.Button as Button exposing (Button)
import UI.Palette as Palette exposing (brightnessMiddle, toneGray, tonePrimary)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Text as Text
import UI.TextField as TextField exposing (TextField)


type alias Config msg =
    { emailField : TextField msg
    , passwordField : TextField msg
    , logoSrc : String
    , title : String
    , submitMsg : msg
    , submitButton : Button msg
    }


titleSpace : RenderConfig -> Int
titleSpace cfg =
    if RenderConfig.isMobile cfg then
        132

    else
        152


view : RenderConfig -> Config msg -> Element msg
view renderConfig { title, logoSrc, emailField, passwordField, submitMsg, submitButton } =
    Element.column
        [ Element.centerY
        , Element.centerX
        , Element.spacingXY 0 24
        , Element.padding 32
        , Element.width (fill |> maximum 384)
        ]
        [ Element.column
            [ Element.height (shrink |> minimum (titleSpace renderConfig))
            , Element.spaceEvenly
            , Element.spacingXY 0 0
            ]
            [ Element.image []
                { src = logoSrc
                , description = "logo"
                }
            , title
                |> String.split "\n"
                |> Text.multiline Text.heading5
                |> Text.withColor (Palette.color tonePrimary brightnessMiddle)
                |> Text.renderElement renderConfig
                |> Element.el
                    [ Element.paddingXY 0 20
                    , Element.width fill
                    ]
            ]
        , emailField
            |> TextField.withPlaceholder "jon@paack.co"
            |> TextField.setLabelVisible True
            |> TextField.withWidth TextField.widthFull
            |> TextField.renderElement renderConfig
        , passwordField
            |> TextField.withPlaceholder "********"
            |> TextField.setLabelVisible True
            |> TextField.withWidth TextField.widthFull
            |> TextField.withOnEnterPressed submitMsg
            |> TextField.renderElement renderConfig
        , Element.column
            [ Element.paddingXY 0 152 ]
            [ submitButton
                |> Button.renderElement renderConfig
            ]
        ]
