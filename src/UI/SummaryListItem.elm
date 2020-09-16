module UI.SummaryListItem exposing (view)

import Element exposing (Element, fill)
import Element.Keyed as Keyed
import UI.Badge as Badge exposing (Badge)
import UI.Palette as Palette
    exposing
        ( brightnessLight
        , brightnessLighter
        , brightnessMiddle
        , toneGray
        , tonePrimary
        )
import UI.RenderConfig exposing (RenderConfig)
import UI.Text as Text exposing (ellipsize)


type alias ColorsHelper =
    { badge : Badge -> Badge
    , title : Palette.Color
    , caption : Palette.Color
    }


view : RenderConfig -> Bool -> String -> String -> Badge -> Element msg
view renderConfig isSelected title caption badge =
    let
        colors =
            listItemColors isSelected
    in
    Keyed.row
        [ Element.width fill
        , Element.paddingEach { top = 11, bottom = 11, left = 20, right = 12 }
        ]
        [ ( "label", listItemLabel renderConfig colors title caption )
        , ( "badge", listItemBadge renderConfig <| colors.badge badge )
        ]


listItemLabel : RenderConfig -> ColorsHelper -> String -> String -> Element msg
listItemLabel renderConfig colors title caption =
    Keyed.column
        [ Element.width fill, Element.clipX, Element.spacing 4 ]
        [ ( "title", listItemTitle renderConfig colors.title title )
        , ( "caption", listItemCaption renderConfig colors.caption caption )
        ]


listItemBadge : RenderConfig -> Badge -> Element msg
listItemBadge renderConfig badge =
    badge
        |> Badge.renderElement renderConfig
        |> Element.el [ Element.alignTop ]


listItemTitle : RenderConfig -> Palette.Color -> String -> Element msg
listItemTitle renderConfig color title =
    Text.body1 title
        |> Text.withOverflow ellipsize
        |> Text.withColor color
        |> Text.renderElement renderConfig
        |> Element.el
            [ Element.width fill ]


listItemCaption : RenderConfig -> Palette.Color -> String -> Element msg
listItemCaption renderConfig color caption =
    Text.caption caption
        |> Text.withOverflow ellipsize
        |> Text.withColor color
        |> Text.renderElement renderConfig


listItemColors : Bool -> ColorsHelper
listItemColors isSelected =
    if isSelected then
        { badge = Badge.withTone Badge.primaryDark
        , title =
            Palette.color tonePrimary brightnessMiddle
                |> Palette.setContrasting True
        , caption = Palette.color tonePrimary brightnessLight
        }

    else
        { badge = identity
        , title =
            Palette.color tonePrimary brightnessLighter
                |> Palette.setContrasting True
        , caption = Palette.color toneGray brightnessMiddle
        }
