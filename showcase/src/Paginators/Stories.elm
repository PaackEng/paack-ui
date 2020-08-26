module Paginators.Stories exposing (stories, update)

import Element exposing (Element)
import Model exposing (Model)
import Msg exposing (Msg)
import Paginators.Model as Paginators
import Paginators.Msg as Paginators
import PluginOptions exposing (defaultWithoutMenu)
import Return exposing (Return)
import UI.Paginator as Paginator
import UI.RenderConfig exposing (RenderConfig)
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


defaultPageSize : Int
defaultPageSize =
    10


update : Paginators.Msg -> Paginators.Model -> Return Paginators.Msg Paginators.Model
update msg model =
    case msg of
        Paginators.PreviousPage ->
            ( { model | offset = model.offset - defaultPageSize }, Cmd.none )

        Paginators.NextPage ->
            ( { model | offset = model.offset + defaultPageSize }, Cmd.none )


stories : RenderConfig -> ExplorerUI
stories cfg =
    storiesOf
        "Paginators"
        [ nonNumericPaginator cfg
        ]


nonNumericPaginator : RenderConfig -> ExplorerStory
nonNumericPaginator cfg =
    storyWithModel
        ( "NonNumeric"
        , view cfg
        , { defaultWithoutMenu
            | code = code
            , note = goToDocsCallToAction "Paginator"
          }
        )


code : String
code =
    prettifyElmCode """
Paginator.nonNumeric
    { onNextButtonClicked = NextPage
    , onPreviousButtonClicked = PreviousPage
    , totalCount = List.length paginatorsStories.items
    , offset = paginatorsStories.offset
    , first = defaultPageSize
    }
    renderConfig
"""


view : RenderConfig -> Model -> Element Msg
view renderConfig { paginatorsStories } =
    Element.column
        [ Element.spacing 20 ]
        [ iconsSvgSprite
        , Paginator.nonNumeric
            { onNextButtonClicked = Msg.PaginatorsStoriesMsg Paginators.NextPage
            , onPreviousButtonClicked = Msg.PaginatorsStoriesMsg Paginators.PreviousPage
            , totalCount = List.length paginatorsStories.items
            , offset = paginatorsStories.offset
            , first = defaultPageSize
            }
            renderConfig
        , paginatorsStories.items
            |> List.take (paginatorsStories.offset + defaultPageSize)
            |> List.drop paginatorsStories.offset
            |> List.map (\item -> Element.text item)
            |> Element.column []
        ]
