module Sidebar.Stories exposing (stories, update)

import Element exposing (Element, fill, height, maximum, px)
import Model exposing (Model)
import Msg exposing (Msg)
import PluginOptions exposing (defaultWithoutMenu)
import Return exposing (Return)
import Sidebar.Model as SidebarModel
import Sidebar.Msg as SidebarMsg
import Tables.Book exposing (Book, books)
import UI.Badge as Badge
import UI.Icon as Icon
import UI.Internal.Menu as Menu
import UI.Internal.NavigationContainer
import UI.Internal.SideBar as Sidebar
import UI.Layout.SplitSelectable as SplitSelectable
import UI.Link as Link
import UI.ListView as ListView exposing (ListView)
import UI.NavigationContainer as Nav
import UI.Palette as Palette exposing (brightnessLighter, tonePrimary)
import UI.RenderConfig exposing (RenderConfig)
import UI.SummaryListItem as Summary
import UI.Text as Text
import UIExplorer exposing (storiesOf)
import Utils
    exposing
        ( ExplorerStory
        , ExplorerUI
        , iconsSvgSprite
        , prettifyElmCode
        , story
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
        [ demo cfg
        ]


demo : RenderConfig -> ExplorerStory
demo renderConfig =
    storyWithModel
        ( "Sidebar"
        , view renderConfig
        , { defaultWithoutMenu | code = code }
        )


view : RenderConfig -> Model -> Element Msg
view renderConfig model =
    Element.column [ height (px 600) ]
        [ iconsSvgSprite
        , Sidebar.desktopColumn renderConfig Element.none <| menu model
        ]


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
        , logo = Nothing
        }


code : String
code =
    prettifyElmCode """
Sidebar.desktopColumn
    renderConfig
    (view model)
    menuConfig
"""
