module ListView.Stories exposing (stories, update)

import Element exposing (Element, fill, height, px, width)
import Html
import Html.Attributes exposing (src)
import ListView.Model as ListViewModel
import ListView.Msg as ListViewMsg
import Model exposing (Model)
import Msg exposing (Msg)
import PluginOptions exposing (defaultWithMenu)
import Return exposing (Return)
import Tables.Book exposing (Book)
import UI.Button as Button
import UI.Icon as Icon
import UI.Internal.Menu as Menu
import UI.Internal.SideBar as Sidebar
import UI.Link as Link
import UI.ListView as ListView
import UI.RenderConfig exposing (RenderConfig)
import UIExplorer exposing (storiesOf)
import Utils
    exposing
        ( ExplorerStory
        , ExplorerUI
        , iconsSvgSprite
        , prettifyElmCode
        , storyBorder
        , storyWithModel
        )


update : ListViewMsg.Msg -> ListViewModel.Model -> Return ListViewMsg.Msg ListViewModel.Model
update msg model =
    case msg of
        ListViewMsg.NoOp ->
            ( model, Cmd.none )


view : RenderConfig -> Model -> Element Msg
view renderConfig model =
    ListView.selectList ListViewMsg.SelectElement
        elementToKey
        elementView
        |> ListView.withItems model.myListElements
        |> ListView.withSearchField
            { label = "Search for elements matching name.."
            , searchMsg = ListViewMsg.FilterSet
            , currentFilter =
                Maybe.map
                    (\str -> ( str, elementHasString ))
                    model.currentFilter
            }
        |> ListView.withSelected
            (\{ id } ->
                Maybe.map (.id >> (==) id) model.selectedElement
                    |> Maybe.withDefault False
            )
        |> ListView.withWidth Element.fill
        |> ListView.renderElement renderConfig


elementHasString : String -> Book -> Bool
elementHasString str { name } =
    String.contains str name
