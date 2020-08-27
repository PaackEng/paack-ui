module Utils exposing
    ( ExplorerModel
    , ExplorerStory
    , ExplorerUI
    , goToDocsCallToAction
    , iconsSvgSprite
    , prettifyElmCode
    , reducedToDocs
    , story
    , storyList
    , storyWithModel
    )

import Element exposing (Element, layout, spacing, wrappedRow)
import Html exposing (Html)
import Model exposing (Model)
import Msg exposing (Msg)
import PluginOptions exposing (PluginOptions, defaultWithMenu)
import UIExplorer


type alias ExplorerModel =
    UIExplorer.Model Model Msg PluginOptions


type alias ExplorerStory =
    ( String, ExplorerModel -> Html Msg, PluginOptions )


type alias ExplorerUI =
    UIExplorer.UI Model Msg PluginOptions


story :
    ( String, Element Msg, PluginOptions )
    -> ExplorerStory
story ( title, content, note ) =
    ( title
    , \_ ->
        layout [] content
    , note
    )


storyList :
    ( String, List (Element Msg), PluginOptions )
    -> ExplorerStory
storyList ( title, content, note ) =
    ( title
    , \_ ->
        layout [] <|
            wrappedRow [ spacing 15 ] content
    , note
    )


storyWithModel :
    ( String, Model -> Element Msg, PluginOptions )
    -> ExplorerStory
storyWithModel ( title, content, note ) =
    ( title
    , \{ customModel } -> layout [] <| content customModel
    , note
    )


iconsSvgSprite : Element msg
iconsSvgSprite =
    Element.html
        (Html.node "paack-svg-icon-sprite"
            []
            []
        )


prettifyElmCode : String -> String
prettifyElmCode code =
    "```elm\n" ++ code ++ "\n```"


goToDocsCallToAction : String -> String
goToDocsCallToAction modulePath =
    "For more information, check the [docs](https://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-" ++ modulePath ++ ")."


reducedToDocs : String -> PluginOptions
reducedToDocs path =
    { defaultWithMenu
        | note = goToDocsCallToAction path
        , code =
            "See [docs](https://package.elm-lang.org/packages/PaackEng/paack-ui/latest/UI-"
                ++ path
                ++ ") for the exact code of this example."
    }
