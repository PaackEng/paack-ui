module UI.Text exposing
    ( Text
    , body1, body2
    , heading1, heading2, heading3, heading4, heading5, heading6
    , subtitle1, subtitle2
    , caption, overline
    , multiline, combination
    , withColor
    , withOverflow, ellipsize, ellipsizeWithTooltip, wrap
    , renderElement
    )

{-| `UI.Text` is a component to specify how text to display text. It applies font size, weight, letter-spacing, and color.

**We discourage the usage of `Element.text` and recommend you to pursuie always using this one instead.**

A text can be created and rendered as in the following pipeline:

    "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
        |> Text.body1
        |> Text.withColor
            (Palette.color tonePrimary brightnessDarkest)
        |> Text.withOverflow ellipsize
        |> Text.renderElement renderConfig
        |> Element.el [ Element.width (px 200) ]

**Note**: Don't forget to import "Fira Sans" family, with the weights: 400, 500, 600 and 700.


# Building

@docs Text


## Body text

@docs body1, body2


## Heading

@docs heading1, heading2, heading3, heading4, heading5, heading6


## Subtitle

@docs subtitle1, subtitle2


## Other

@docs caption, overline


# Combine

@docs multiline, combination


# Color

@docs withColor


# Overflow

@docs withOverflow, ellipsize, ellipsizeWithTooltip, wrap


# Rendering

@docs renderElement

-}

import Element exposing (Element)
import List
import UI.Internal.Text as Internal exposing (TextOverflow, TextSize(..), defaultText, mapOptions)
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)


{-| The `Text` type is used for describing the component for later rendering.
-}
type alias Text =
    Internal.Text


{-| The biggest title size.
-}
heading1 : String -> Text
heading1 content =
    defaultText SizeHeading1 content


{-| A size for titles.
-}
heading2 : String -> Text
heading2 content =
    defaultText SizeHeading2 content


{-| A size for titles.
-}
heading3 : String -> Text
heading3 content =
    defaultText SizeHeading3 content


{-| A size for titles.
-}
heading4 : String -> Text
heading4 content =
    defaultText SizeHeading4 content


{-| A size for titles.
-}
heading5 : String -> Text
heading5 content =
    defaultText SizeHeading5 content


{-| A size for titles.
-}
heading6 : String -> Text
heading6 content =
    defaultText SizeHeading6 content


{-| The biggest size for subtitles.
-}
subtitle1 : String -> Text
subtitle1 content =
    defaultText SizeSubtitle1 content


{-| The smallest size for subtitles.
-}
subtitle2 : String -> Text
subtitle2 content =
    defaultText SizeSubtitle2 content


{-| The biggest size for paragraphs.
-}
body1 : String -> Text
body1 content =
    defaultText SizeBody1 content


{-| The smallest size for paragraphs.
-}
body2 : String -> Text
body2 content =
    defaultText SizeBody2 content


{-| For writing captions.
-}
caption : String -> Text
caption content =
    defaultText SizeCaption content


{-| For writing labels.
-}
overline : String -> Text
overline content =
    defaultText SizeOverline content


{-| Text colors can variate to varite context or contrast with a background.
See [`Palette.color`](UI-Palette#color) and [`Palette.setContrasting`](UI-Palette#setContrasting) for how to compose a valid color value.

    "Action completed with success!"
        |> Text.body1
        |> Text.withColor
            (Palette.color Palette.toneSuccess Palette.brightnessMiddle)
        |> Text.renderElement renderConfig

-}
withColor : Palette.Color -> Text -> Text
withColor color text =
    mapOptions (\opt -> { opt | color = Internal.ColorPalette color }) text


{-| Truncates the text and adds the ellipsis.
-}
ellipsize : TextOverflow
ellipsize =
    Internal.Ellipsize


{-| Truncates the text, adds the ellipsis and displays a tooltip with the whole content.
-}
ellipsizeWithTooltip : TextOverflow
ellipsizeWithTooltip =
    Internal.EllipsizeWithTooltip


{-| Lets the text break lines to prevent overflow.
Default behavior.
-}
wrap : TextOverflow
wrap =
    Internal.Wrap


{-| Determines how the text overflow is handled.

    "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
        |> Text.heading3
        |> Text.withOverflow ellipsize
        |> Text.renderElement renderConfig
        |> Element.el [ Element.width (px 42) ]

-}
withOverflow : TextOverflow -> Text -> Text
withOverflow overflow text =
    Internal.withOverflow overflow text


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Text -> Element msg
renderElement cfg (Internal.Text spans opt) =
    case spans of
        [] ->
            Element.none

        [ theOne ] ->
            Internal.spanRenderEl cfg opt theOne

        _ ->
            List.map (Internal.spanRenderEl cfg opt) spans
                |> Element.column (Internal.combinedAttrs opt)


{-| Combines lines of text with the same styling.

    [ "First line of text."
    , "Second line."
    , "Last line."
    ]
        |> Text.multiline Text.body1
        |> Text.renderElement renderConfig

-}
multiline : (String -> Text) -> List String -> Text
multiline style lines =
    let
        newSpans =
            List.concatMap (style >> Internal.getSpans) lines
    in
    Internal.Text newSpans Internal.textDefaultOptions


{-| Combines lines of text with the different styling.

    [ Text.heading6 "First line of text."
    , Text.body1 "Second line."
    , Text.caption "Last line."
    ]
        |> Text.combination
        |> Text.renderElement renderConfig

-}
combination : List Text -> Text
combination parts =
    let
        newSpans =
            List.concatMap Internal.getSpans parts
    in
    Internal.Text newSpans Internal.textDefaultOptions
