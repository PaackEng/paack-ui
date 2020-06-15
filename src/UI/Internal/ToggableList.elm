module UI.Internal.ToggableList exposing (Config, Row, view)

import Element exposing (Attribute, Element, fill, px)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import UI.Icon as Icon
import UI.Internal.Basics exposing (ifThenElse)
import UI.Palette as Palette exposing (brightnessDarkest, brightnessLight, brightnessLightest, brightnessMiddle, toneGray, tonePrimary)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.SelectList as SelectList exposing (selectList)
import UI.Text as Text
import UI.Utils.Element exposing (zeroPadding)


type alias Config =
    { detailsShowLabel : String
    , detailsCollapseLabel : String
    }


type alias Row object msg =
    { details : List ( String, Element msg )
    , mainLabel : String
    , mainContent : Element msg
    , id : object
    }


view : RenderConfig -> Config -> (object -> msg) -> (object -> Bool) -> List (Row object msg) -> Element msg
view renderConfig config select isSelected items =
    let
        itemView parentCfg selected item =
            if selected then
                selectedRow parentCfg config item

            else
                defaultRow parentCfg config selected item
    in
    selectList (\{ id } -> select id) itemView
        |> SelectList.withOptions items
        |> SelectList.withSelected (\{ id } -> isSelected id)
        |> SelectList.toEl renderConfig


defaultRow : RenderConfig -> Config -> Bool -> Row object msg -> Element msg
defaultRow renderConfig config selected { mainLabel, mainContent } =
    Element.row
        [ Element.width fill
        , Element.paddingEach { top = 11, bottom = 11, left = 28, right = 12 }
        ]
        [ [ Text.body1 mainLabel
                |> Text.withColor (titleColor selected)
                |> Text.setEllipsis True
                |> Text.toEl renderConfig
          , mainContent
          ]
            |> Element.column [ Element.width fill, Element.clipX ]
        , ifThenElse selected
            (Icon.toggleUp config.detailsCollapseLabel)
            (Icon.toggleDown config.detailsShowLabel)
            |> Icon.withColor (titleColor selected)
            |> Icon.toEl renderConfig
            |> Element.el
                [ Font.center
                , Element.width (px 32)
                , Element.paddingXY 0 6
                ]
        ]


selectedRow : RenderConfig -> Config -> Row object msg -> Element msg
selectedRow renderConfig config ({ mainLabel, mainContent, details } as item) =
    Element.column [ Element.width fill ]
        [ defaultRow renderConfig config True item
        , details
            |> List.map (someCard renderConfig)
            |> Element.column toggleableCard
        ]


someCard : RenderConfig -> ( String, Element msg ) -> Element msg
someCard renderConfig ( label, content ) =
    Element.column identCard
        [ label
            |> Text.overline
            |> Text.withColor (Palette.color toneGray brightnessLight)
            |> Text.setEllipsis True
            |> Text.toEl renderConfig
        , content
        ]


identCard : List (Attribute msg)
identCard =
    [ Element.paddingEach { zeroPadding | left = 8 }
    , Border.widthEach { zeroPadding | left = 2 }
    , Palette.color tonePrimary brightnessMiddle
        |> Palette.toElColor
        |> Border.color
    , Element.width fill
    ]


toggleableCard : List (Attribute msg)
toggleableCard =
    [ Element.paddingEach { top = 16, bottom = 19, left = 28, right = 20 }
    , Palette.color toneGray brightnessLightest
        |> Palette.toElColor
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
