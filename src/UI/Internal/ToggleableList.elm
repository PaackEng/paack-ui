module UI.Internal.ToggleableList exposing (Config, Cover, defaultRow, selectedRow)

import Element exposing (Attribute, Element, fill, px)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Keyed as Keyed
import UI.Icon as Icon
import UI.Internal.Basics exposing (ifThenElse)
import UI.Palette as Palette exposing (brightnessDarkest, brightnessLight, brightnessLighter, brightnessLightest, brightnessMiddle, toneGray, tonePrimary)
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text exposing (ellipsize)
import UI.Utils.Element exposing (zeroPadding)


type alias Config object msg =
    { detailsShowLabel : String
    , detailsCollapseLabel : String
    , toCover : object -> Cover
    , toDetails : object -> List ( String, Element msg )
    , selectMsg : object -> msg
    , toKey : object -> String
    }


type alias Cover =
    { title : String, caption : Maybe String }


defaultRow : RenderConfig -> Config object msg -> Bool -> object -> Element msg
defaultRow renderConfig config selected object =
    Element.row
        [ Element.width fill
        , Element.paddingEach { top = 11, bottom = 11, left = 28, right = 12 }
        , Element.height Element.shrink
        ]
        [ coverView renderConfig (config.toCover object) selected
            |> Element.el
                [ Element.width fill
                , Element.centerY
                , Element.clipX
                , Element.paddingXY 0 5
                ]
        , ifThenElse selected
            (Icon.toggleUp config.detailsCollapseLabel)
            (Icon.toggleDown config.detailsShowLabel)
            |> Icon.withSize Size.large
            |> Icon.withColor (titleColor selected)
            |> Icon.renderElement renderConfig
            |> Element.el
                [ Font.center
                , Element.width (px 32)
                , Element.paddingXY 0 6
                , Element.centerY
                ]
        ]


selectedRow : RenderConfig -> Config object msg -> object -> Element msg
selectedRow renderConfig config object =
    Element.column [ Element.width fill ]
        [ defaultRow renderConfig config True object
        , object
            |> config.toDetails
            |> List.map (detailItem renderConfig)
            |> Keyed.column toggleableCard
        ]


detailItem : RenderConfig -> ( String, Element msg ) -> ( String, Element msg )
detailItem renderConfig ( label, content ) =
    ( label
    , Element.column indentedDetailItemAttributes
        [ label
            |> Text.overline
            |> Text.withColor (Palette.color toneGray brightnessLight)
            |> Text.withOverflow ellipsize
            |> Text.renderElement renderConfig
        , content
        ]
    )


indentedDetailItemAttributes : List (Attribute msg)
indentedDetailItemAttributes =
    [ Element.paddingEach { zeroPadding | left = 8 }
    , Border.widthEach { zeroPadding | left = 2 }
    , Palette.color tonePrimary brightnessMiddle
        |> Palette.toElementColor
        |> Border.color
    , Element.width fill
    ]


toggleableCard : List (Attribute msg)
toggleableCard =
    [ Element.paddingEach { top = 16, bottom = 19, left = 28, right = 20 }
    , Palette.color toneGray brightnessLightest
        |> Palette.toElementColor
        |> Background.color
    , Element.width fill
    , Element.spacing 12
    ]


titleColor : Bool -> Palette.Color
titleColor selected =
    if selected then
        Palette.color tonePrimary brightnessMiddle |> Palette.setContrasting True

    else
        Palette.color toneGray brightnessDarkest


captionColor : Bool -> Palette.Color
captionColor selected =
    if selected then
        Palette.color tonePrimary brightnessLighter

    else
        Palette.color toneGray brightnessLight


coverView : RenderConfig -> Cover -> Bool -> Element msg
coverView cfg { title, caption } selected =
    let
        titleComponent =
            Text.body1 title
                |> Text.withColor (titleColor selected)

        captionApplied =
            case caption of
                Just captionStr ->
                    Text.combination
                        [ titleComponent
                        , Text.caption captionStr
                            |> Text.withColor (captionColor selected)
                        ]

                Nothing ->
                    titleComponent
    in
    captionApplied
        |> Text.withOverflow ellipsize
        |> Text.renderElement cfg
        |> Element.el [ Element.width fill, Element.clipX ]
