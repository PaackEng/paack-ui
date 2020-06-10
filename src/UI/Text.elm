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
    , setEllipsis
    , subtitle1
    , subtitle2
    , toEl
    , withColor
    )

import Element exposing (Element)
import List
import UI.Internal.Text as Internal exposing (TextSize(..), defaultText, mapOptions)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)


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


setEllipsis : Bool -> Text -> Text
setEllipsis val text =
    Internal.setEllipsis val text


toEl : RenderConfig -> Text -> Element msg
toEl cfg (Internal.Text spans opt) =
    case spans of
        [] ->
            Element.none

        [ theOne ] ->
            Internal.spanRenderEl cfg theOne

        _ ->
            List.map (Internal.spanRenderEl cfg) spans
                |> Element.column (Internal.combinedAttrs opt)


multiline : (String -> Text) -> List String -> Text
multiline style lines =
    let
        newSpans =
            lines
                |> List.map (style >> Internal.getSpans)
                |> List.concat
    in
    Internal.Text newSpans Internal.textDefaultOptions


combination : List Text -> Text
combination parts =
    let
        newSpans =
            parts
                |> List.map Internal.getSpans
                |> List.concat
    in
    Internal.Text newSpans Internal.textDefaultOptions
