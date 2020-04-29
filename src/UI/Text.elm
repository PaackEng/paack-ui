module UI.Text exposing
    ( Text
    , TextSize
    , body1
    , body2
    , caption
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
    )

import Element exposing (Attribute, Element)
import Element.Font as Font
import List


type alias Properties =
    { content : String
    , size : TextSize
    }


type Text
    = Text Properties


type TextSize
    = SizeHeading1
    | SizeHeading2
    | SizeHeading3
    | SizeHeading4
    | SizeHeading5
    | SizeHeading6
    | SizeSubtitle1
    | SizeSubtitle2
    | SizeBody1
    | SizeBody2
    | SizeCaption
    | SizeOverline


heading1 : String -> Text
heading1 content =
    Text (Properties content SizeHeading1)


heading2 : String -> Text
heading2 content =
    Text (Properties content SizeHeading2)


heading3 : String -> Text
heading3 content =
    Text (Properties content SizeHeading3)


heading4 : String -> Text
heading4 content =
    Text (Properties content SizeHeading4)


heading5 : String -> Text
heading5 content =
    Text (Properties content SizeHeading5)


heading6 : String -> Text
heading6 content =
    Text (Properties content SizeHeading6)


subtitle1 : String -> Text
subtitle1 content =
    Text (Properties content SizeSubtitle1)


subtitle2 : String -> Text
subtitle2 content =
    Text (Properties content SizeSubtitle2)


body1 : String -> Text
body1 content =
    Text (Properties content SizeBody1)


body2 : String -> Text
body2 content =
    Text (Properties content SizeBody2)


caption : String -> Text
caption content =
    Text (Properties content SizeCaption)


overline : String -> Text
overline content =
    Text (Properties content SizeOverline)


toEl : Text -> Element msg
toEl text =
    case text of
        Text { content, size } ->
            content
                |> Element.text
                |> List.singleton
                |> Element.paragraph (deskAttributes size)



-- Internal


deskAttributes : TextSize -> List (Attribute msg)
deskAttributes size =
    case size of
        SizeHeading1 ->
            [ Font.size 92 ]

        SizeHeading2 ->
            [ Font.size 64 ]

        SizeHeading3 ->
            [ Font.size 48 ]

        SizeHeading4 ->
            [ Font.size 32 ]

        SizeHeading5 ->
            [ Font.size 24 ]

        SizeHeading6 ->
            [ Font.size 20 ]

        SizeSubtitle1 ->
            [ Font.size 16 ]

        SizeSubtitle2 ->
            [ Font.size 14 ]

        SizeBody1 ->
            [ Font.size 16, Element.spacing 24 ]

        SizeBody2 ->
            [ Font.size 14, Element.spacing 24 ]

        SizeCaption ->
            [ Font.size 12, Element.spacing 16 ]

        SizeOverline ->
            [ Font.size 10 ]
