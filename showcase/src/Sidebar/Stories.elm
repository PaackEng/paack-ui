module Sidebar.Stories exposing (stories, update)

import Element exposing (Element, height, px, width)
import Html
import Html.Attributes exposing (src)
import Model exposing (Model)
import Msg exposing (Msg)
import PluginOptions exposing (defaultWithMenu)
import Return exposing (Return)
import Sidebar.Model as SidebarModel
import Sidebar.Msg as SidebarMsg
import UI.Icon as Icon
import UI.Internal.Menu as Menu
import UI.Internal.SideBar as Sidebar
import UI.Link as Link
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


update : SidebarMsg.Msg -> SidebarModel.Model -> Return SidebarMsg.Msg SidebarModel.Model
update msg model =
    case msg of
        SidebarMsg.ToggleSidebar expanded ->
            ( { model | expanded = expanded }, Cmd.none )

        SidebarMsg.NoOp ->
            ( model, Cmd.none )


stories : RenderConfig -> ExplorerUI
stories cfg =
    storiesOf
        "Sidebar"
        [ persistentStory cfg
        , nonPersistentStory cfg
        ]


persistentStory : RenderConfig -> ExplorerStory
persistentStory renderConfig =
    storyWithModel
        ( "Persistent"
        , persistentView renderConfig
        , { defaultWithMenu | code = persistentSidebarCode }
        )


nonPersistentStory : RenderConfig -> ExplorerStory
nonPersistentStory renderConfig =
    storyWithModel
        ( "Non-persistent"
        , nonPersistentView renderConfig
        , { defaultWithMenu | code = nonPersistentSidebarCode }
        )


persistentView : RenderConfig -> Model -> Element Msg
persistentView renderConfig model =
    Element.column [ height (px 600) ]
        [ iconsSvgSprite
        , Sidebar.desktopPersistent renderConfig page <| menu model
        ]


nonPersistentView : RenderConfig -> Model -> Element Msg
nonPersistentView renderConfig model =
    Element.column [ height (px 600) ]
        [ iconsSvgSprite
        , Sidebar.desktopNonPersistent renderConfig page <| menu model
        ]


page : Element Msg
page =
    storyBorder <|
        Element.el [ width (px 800), height (px 600) ] Element.none


menu : Model -> Menu.Menu Msg
menu model =
    Menu.Menu
        (Menu.Properties (Msg.SidebarStoriesMsg << SidebarMsg.ToggleSidebar)
            model.sidebarStories.expanded
        )
        { pages =
            [ { labeledIcon = Icon.notifications "Notifications"
              , link = Link.link "/notifications"
              , isCurrent = False
              }
            , { labeledIcon = Icon.toggle "Routes"
              , link = Link.link "/routes"
              , isCurrent = True
              }
            ]
        , actions =
            [ { labeledIcon = Icon.logout "Logout"
              , action = Msg.SidebarStoriesMsg <| SidebarMsg.NoOp
              }
            ]
        , logo =
            Just
                { hint = "Paack logo"
                , body = Element.html <| Html.img [ Html.Attributes.width 84, src "logo.svg" ] []
                }
        }


persistentSidebarCode : String
persistentSidebarCode =
    prettifyElmCode """
Sidebar.desktopPersistent
    renderConfig
    (view model)
    (menuConfig model)
"""


nonPersistentSidebarCode : String
nonPersistentSidebarCode =
    prettifyElmCode """
Sidebar.desktopNonPersistent
    renderConfig
    (view model)
    (menuConfig model)
"""
