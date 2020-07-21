module Utils exposing
    ( goToDocsCallToAction
    , iconsSvgSprite
    , prettifyElmCode
    , story
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


iconsSvgSprite =
    Element.html
        (Html.node "paack-svg-icon-sprite"
            []
            []
        )


prettifyElmCode : String -> String
prettifyElmCode code =
    "```elm" ++ code ++ "\n```"


goToDocsCallToAction : String -> String
goToDocsCallToAction modulePath =
    "For more information, check the [docs](https://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-" ++ modulePath ++ ")."
