module Utils exposing
    ( story
    , storyList
    , storyWithModel
    )

import Element exposing (Element, layout, spacing, wrappedRow)
import Html exposing (Html)
import Model exposing (Model)


story ( title, content, note ) =
    ( title
    , \_ ->
        layout [] content
    , note
    )


storyList ( title, content, note ) =
    ( title
    , \_ ->
        layout [] <|
            wrappedRow [ spacing 15 ] content
    , note
    )


storyWithModel ( title, content, note ) =
    ( title
    , \{ customModel } -> layout [] <| content customModel
    , note
    )
