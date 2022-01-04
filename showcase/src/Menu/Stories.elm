module Menu.Stories exposing (stories, update)

import Element exposing (Element, fill)
import Menu.Model as Menu
import Menu.Msg as Menu
import Model exposing (Model)
import Msg exposing (Msg)
import PluginOptions exposing (defaultWithoutMenu)
import Return exposing (Return)
import UI.Button as Button
import UI.Icon as Icon
import UI.Menu as Menu
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


update : Menu.Msg -> Menu.Model -> Return Msg.Msg Menu.Model
update msg model =
    case msg of
        Menu.ToggleMenu ->
            ( { model | isVisible = not model.isVisible }, Cmd.none )


stories : RenderConfig -> ExplorerUI
stories renderConfig =
    storiesOf
        "Menu"
        [ menuDemo renderConfig
        ]


menuDemo : RenderConfig -> ExplorerStory
menuDemo renderConfig =
    storyWithModel
        ( "Menu"
        , view renderConfig
        , { defaultWithoutMenu
            | code = menuCode
            , note = goToDocsCallToAction "Menu"
          }
        )


toggleMenuMsg : Msg
toggleMenuMsg =
    Msg.MenuStoriesMsg Menu.ToggleMenu


view : RenderConfig -> Model -> Element Msg
view renderConfig { menuStories } =
    Element.column
        [ Element.spacing 8, Element.width fill ]
        [ iconsSvgSprite
        , "Menu"
            |> Icon.sandwichMenu
            |> Button.fromIcon
            |> Button.cmd toggleMenuMsg Button.primary
            |> Menu.menu toggleMenuMsg
                [ "Edit"
                    |> Menu.item toggleMenuMsg Nothing
                , "Download"
                    |> Menu.item toggleMenuMsg (Just Icon.download)
                , "Delete"
                    |> Menu.item toggleMenuMsg (Just Icon.delete)
                    |> Menu.itemWithDangerTone
                ]
            |> Menu.setVisible menuStories.isVisible
            |> Menu.renderElement renderConfig
        ]


menuCode : String
menuCode =
    prettifyElmCode """
    "Menu"
        |> Icon.sandwichMenu
        |> Button.fromIcon
        |> Button.cmd toggleMenuMsg Button.primary
        |> Menu.menu toggleMenuMsg
            [ "Edit"
                |> Menu.item toggleMenuMsg (Just Icon.edit)
            , "Download"
                |> Menu.item toggleMenuMsg (Just Icon.download)
            ,  "Delete"
                |> Menu.item toggleMenuMsg (Just Icon.delete)
                |> Menu.itemWithDangerTone
            ]
        |> Menu.setVisible menuStories.isVisible
        |> Menu.renderElement renderConfig
"""
