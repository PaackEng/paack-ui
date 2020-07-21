module Paginators.Stories exposing (stories, update)

import Element exposing (fill)
import Msg
import Paginators.Model as Paginators
import Paginators.Msg as Paginators
import Return as R exposing (Return)
import UI.Paginator as Paginator
import UIExplorer exposing (storiesOf)
import Utils exposing (iconsSvgSprite, prettifyElmCode, story, storyWithModel)


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


stories cfg =
    storiesOf
        "Paginators"
        [ nonNumericPaginator cfg
        ]


nonNumericPaginator cfg =
    storyWithModel
        ( "NonNumeric"
        , \{ paginatorsStories } ->
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
                    cfg
                , paginatorsStories.items
                    |> List.take (paginatorsStories.offset + defaultPageSize)
                    |> List.drop paginatorsStories.offset
                    |> List.map (\item -> Element.text item)
                    |> Element.column []
                ]
        , { code = prettifyElmCode """
Paginator.nonNumeric
    { onNextButtonClicked = NextPage
    , onPreviousButtonClicked = PreviousPage
    , totalCount = List.length paginatorsStories.items
    , offset = paginatorsStories.offset
    , first = defaultPageSize
    }
    cfg
"""
          , note = ""
          , hasMenu = False
          }
        )
