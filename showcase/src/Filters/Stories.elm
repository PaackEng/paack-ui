module Filters.Stories exposing (stories, update)

import Element exposing (Element, fill, maximum, minimum)
import Filters.Model as Story
import Filters.Msg as Story
import Model exposing (Model)
import Msg exposing (Msg)
import PluginOptions exposing (defaultWithMenu)
import Return exposing (Return)
import UI.Button as Button
import UI.Filter as Filter
import UI.ListView as ListView
import UI.RenderConfig exposing (RenderConfig)
import UI.Size as Size
import UI.Text as Text
import UI.TextField as TextField
import UIExplorer exposing (storiesOf)
import Utils
    exposing
        ( ExplorerStory
        , ExplorerUI
        , goToDocsCallToAction
        , iconsSvgSprite
        , prettifyElmCode
        , storyWithModel
        )


update : Story.Msg -> Story.Model -> Return Msg.Msg Story.Model
update msg model =
    case msg of
        Story.FilterMsg subMsg ->
            Tuple.mapBoth
                (\newFilter -> { model | demoFilter = newFilter })
                (always Cmd.none)
                (Filter.update subMsg model.demoFilter)


stories : RenderConfig -> ExplorerUI
stories renderConfig =
    storiesOf
        "Filter"
        [ medium renderConfig
        , small renderConfig
        , united renderConfig
        ]


medium : RenderConfig -> ExplorerStory
medium renderConfig =
    storyWithModel
        ( "Medium"
        , mediumView renderConfig
        , { defaultWithMenu
            | code = mediumExample
            , note = goToDocsCallToAction "Filter"
          }
        )


mediumView : RenderConfig -> Model -> Element Msg
mediumView renderConfig { filtersStories } =
    Element.column [ Element.width (fill |> maximum 240), Element.height (fill |> minimum 320) ]
        [ iconsSvgSprite
        , Filter.fromModel "Planet Name"
            (Msg.FiltersStoriesMsg << Story.FilterMsg)
            filtersStories.demoFilter
            |> Filter.renderElement renderConfig
        , ListView.simpleList identity
            (always <| Element.el [ Element.padding 8 ] << Text.renderElement renderConfig << Text.body2)
            |> ListView.withItems (Filter.getItems filtersStories.demoFilter)
            |> ListView.renderElement renderConfig
        ]


mediumExample : String
mediumExample =
    prettifyElmCode """
type alias Model =
    { demoFilter : FilterModel Msg String
    }


init =
    { demoFilter =
        Filter.multiTextFilter [] identity
            |> Filter.setItems [ "Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune" ]
    }

view renderConfig model =
    Element.column []
        [ Filter.fromModel "Planet Name"
            Msg.ForFilter
            model.demoFilter
            |> Filter.renderElement renderConfig
        , ListView.simpleList identity
            (always <| Text.renderElement renderConfig << Text.body2)
            |> ListView.withItems (Filter.getItems model.demoFilter)
            |> ListView.renderElement renderConfig
        ]
"""


small : RenderConfig -> ExplorerStory
small renderConfig =
    storyWithModel
        ( "Extra Small"
        , smallView renderConfig
        , { defaultWithMenu
            | code = smallExample
            , note = goToDocsCallToAction "Filter"
          }
        )


smallView : RenderConfig -> Model -> Element Msg
smallView renderConfig { filtersStories } =
    Element.column [ Element.width (fill |> maximum 240), Element.height (fill |> minimum 320) ]
        [ iconsSvgSprite
        , Filter.fromModel "Planet Name"
            (Msg.FiltersStoriesMsg << Story.FilterMsg)
            filtersStories.demoFilter
            |> Filter.withSize Filter.sizeExtraSmall
            |> Filter.renderElement renderConfig
        , ListView.simpleList identity
            (always <| Element.el [ Element.padding 8 ] << Text.renderElement renderConfig << Text.body2)
            |> ListView.withItems (Filter.getItems filtersStories.demoFilter)
            |> ListView.renderElement renderConfig
        ]


smallExample : String
smallExample =
    prettifyElmCode """
type alias Model =
    { demoFilter : FilterModel Msg String
    }


init =
    { demoFilter =
        Filter.multiTextFilter [] identity
            |> Filter.setItems [ "Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune" ]
    }

view renderConfig model =
    Element.column []
        [ Filter.fromModel "Planet Name"
            Msg.ForFilter
            model.demoFilter
            |> Filter.withSize Filter.sizeExtraSmall
            |> Filter.renderElement renderConfig
        , ListView.simpleList identity
            (always <| Text.renderElement renderConfig << Text.body2)
            |> ListView.withItems (Filter.getItems model.demoFilter)
            |> ListView.renderElement renderConfig
        ]
"""


united : RenderConfig -> ExplorerStory
united renderConfig =
    storyWithModel
        ( "United"
        , unitedView renderConfig
        , { defaultWithMenu
            | note = goToDocsCallToAction "Filter"
          }
        )


unitedView : RenderConfig -> Model -> Element Msg
unitedView renderConfig _ =
    Element.wrappedRow [ Element.width fill, Element.height (fill |> minimum 320), Element.spacing 16 ]
        [ iconsSvgSprite
        , Element.column [ Element.width (fill |> maximum 160), Element.spacing 16 ]
            [ Filter.customFilter "Closed"
                { openMsg = Msg.NoOp, closeMsg = Msg.NoOp, isOpen = False }
                |> Filter.renderElement renderConfig
            , Filter.customFilter "Applied"
                { openMsg = Msg.NoOp, closeMsg = Msg.NoOp, isOpen = False }
                |> Filter.withAppliedHeader
                    (Just <| Filter.appliedHeader "💘" Msg.NoOp)
                |> Filter.renderElement renderConfig
            ]
        , Filter.customFilter "Mambo No. 5"
            { openMsg = Msg.NoOp, closeMsg = Msg.NoOp, isOpen = True }
            |> Filter.withSize Filter.sizeExtraSmall
            |> Filter.withSorting
                (Filter.sorting
                    { sortAscendingMsg = Msg.NoOp
                    , sortDescendingMsg = Msg.NoOp
                    , clearSortingMsg = Msg.NoOp
                    }
                    |> Filter.withAppliedSorting Filter.sortingAscending
                    |> Filter.withSortingPreview { smaller = "Monica", larger = "Jessica" }
                )
            |> Filter.withButtons
                [ Button.disabled <| Button.fromLabel "in my life"
                , Button.disabled <| Button.fromLabel "by my side"
                , Button.disabled <| Button.fromLabel "is all I need"
                ]
            |> Filter.withBody
                [ ""
                    |> TextField.singlelineText (always Msg.NoOp) "A little bit of"
                    |> TextField.withPlaceholder "A little bit of"
                    |> TextField.withSize Size.extraSmall
                    |> TextField.withWidth TextField.widthFull
                    |> TextField.withOnEnterPressed Msg.NoOp
                    |> TextField.renderElement renderConfig
                ]
            |> Filter.renderElement renderConfig
        ]
