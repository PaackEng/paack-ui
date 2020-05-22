module UI.Text exposing
    ( Text
    , TextColor
    , body1
    , body2
    , caption
    , combination
    , heading1
    , heading2
    , heading3
    , heading4
    , heading5
    , heading6
    , multiline
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
import UI.Internal.Text as Internal exposing (TextSize(..), defaultText, mapOptions)
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


withColor : Palette.Color -> Text -> Text
withColor color text =
    mapOptions (\opt -> { opt | color = Internal.ColorPalette color }) text


withEllipsis : Bool -> Text -> Text
withEllipsis val text =
    mapOptions (\opt -> { opt | oneLineEllipsis = val }) text


toEl : RenderConfig -> Text -> Element msg
toEl cfg (Internal.Text spans) =
    case spans of
        [] ->
            Element.none

        [ theOne ] ->
            Internal.spanRenderEl cfg theOne

        _ ->
            -- TODO: Concat paragraphs
            List.map (Internal.spanRenderEl cfg) spans
                |> Element.column []


multiline : (String -> Text) -> List String -> Text
multiline style lines =
    lines
        |> List.map (style >> Internal.getSpans)
        |> List.concat
        |> Internal.Text


combination : List Text -> Text
combination parts =
    parts
        |> List.map Internal.getSpans
        |> List.concat
        |> Internal.Text
