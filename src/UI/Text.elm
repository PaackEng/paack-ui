module UI.Text exposing
    ( Text
    , TextColor
    , body1
    , body2
    , caption
    , colorBgMiddle
    , colorFromPalette
    , colorGray
    , colorInverted
    , colorPrimary
    , heading1
    , heading2
    , heading3
    , heading4
    , heading5
    , heading6
    , overline
    , subtitle1
    , subtitle2
    , toEl
    , withColor
    , withEllipsis
    )

import Element exposing (Attribute, Element)
import Element.Font as Font
import List
import UI.Internal.Text as Internal exposing (TextSize(..), defaultText)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig, isMobile)


type alias Text =
    Internal.Text


type alias TextColor =
    Internal.TextColor


heading1 : String -> Text
heading1 content =
    defaultText SizeHeading1 content


heading2 : String -> Text
heading2 content =
    defaultText SizeHeading2 content


heading3 : String -> Text
heading3 content =
    defaultText SizeHeading3 content


heading4 : String -> Text
heading4 content =
    defaultText SizeHeading4 content


heading5 : String -> Text
heading5 content =
    defaultText SizeHeading5 content


heading6 : String -> Text
heading6 content =
    defaultText SizeHeading6 content


subtitle1 : String -> Text
subtitle1 content =
    defaultText SizeSubtitle1 content


subtitle2 : String -> Text
subtitle2 content =
    defaultText SizeSubtitle2 content


body1 : String -> Text
body1 content =
    defaultText SizeBody1 content


body2 : String -> Text
body2 content =
    defaultText SizeBody2 content


caption : String -> Text
caption content =
    defaultText SizeCaption content


overline : String -> Text
overline content =
    defaultText SizeOverline content


colorPrimary : TextColor
colorPrimary =
    Internal.ColorPalette
        ( Palette.tonePrimary, Palette.lumMiddle )


colorBgMiddle : TextColor
colorBgMiddle =
    Internal.ColorBgMiddle


colorInverted : TextColor
colorInverted =
    Internal.ColorInverted


colorGray : TextColor
colorGray =
    Internal.ColorPalette
        ( Palette.toneGray, Palette.lumMiddle )


colorFromPalette : Palette.Color -> TextColor
colorFromPalette color =
    Internal.ColorPalette color


withColor : TextColor -> Text -> Text
withColor color (Internal.Text prop opt) =
    Internal.Text prop { opt | color = color }


withEllipsis : Bool -> Text -> Text
withEllipsis val (Internal.Text prop opt) =
    Internal.Text prop { opt | oneLineEllipsis = val }


toEl : RenderConfig -> Text -> Element msg
toEl cfg (Internal.Text { content, size } { color, oneLineEllipsis }) =
    let
        attrs =
            Internal.attributes cfg size oneLineEllipsis color
    in
    content
        |> Element.text
        |> List.singleton
        |> Element.paragraph attrs
